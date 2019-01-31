/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async

import user.{AsyncBase, FutureSystem}
import scala.reflect.internal.Flags

// TODO: check there's no await outside of an async block

abstract class AsyncEarlyExpansion extends AsyncContext {
  import u._

  // TODO: remove copy/paste from ExprBuilder (note that this part runs during typer though!)
  lazy val futureSystem: FutureSystem = asyncBase.futureSystem
  lazy val futureSystemOps: futureSystem.Ops[u.type] = futureSystem.mkOps(u, false)

  def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = u.Expr[T](rootMirror, FixedMirrorTreeCreator(rootMirror, tree))
  def WeakTypeTag[T](tpe: Type): WeakTypeTag[T] = u.WeakTypeTag[T](rootMirror, FixedMirrorTypeCreator(rootMirror, tpe))

  def spawn(tree: Tree, execContext: Tree): Tree =
    futureSystemOps.future(Expr[Unit](tree))(Expr[futureSystem.ExecContext](execContext)).tree

  def promiseToFuture(prom: Tree, resTp: Type): Tree =
    futureSystemOps.promiseToFuture(Expr[Nothing](prom))(WeakTypeTag(resTp)).tree

  // During typers, expand async macro to a block that creates the state machine class,
  // along with skeletal def trees, while delaying the expansion of the actual state machine
  // logic (anf & async) until after erasure.
  // The state machine's apply method just has the async macro's RHS `asyncBody` as-is.
  // The goal is to balance compiler performance by delaying tree explosion (due to anf and state machine mechanics) and
  // complexity arising from deftree synthesis in later phases, which would require
  // retro-actively running erasure (bridges/types) and explicitouter (add outer arg to state machine ctor)
  // on the synthetic def trees.
  def apply(asyncBody: Tree, execContext: Tree, resultType: Type, originalOwner: Symbol) = {
    val tryResult = futureSystemOps.tryType(resultType)

    val stateMachine: ClassDef = {
      val parents = {
        val customParents = futureSystemOps.stateMachineClassParents
        // prefer extending a class to reduce the class file size of the state machine.
        // ... unless a custom future system already extends some class
        val useClass = customParents.forall(_.typeSymbol.asClass.isTrait)

        val fun1Class =
          if (useClass) symbolOf[scala.runtime.AbstractFunction1[Any, Any]]
          else symbolOf[scala.Function1[Any, Any]]

        // We extend () => Unit so we can pass this class as the by-name argument to `Future.apply`.
        // See SI-1247 for the the optimization that avoids creation.
        val funParents = List(appliedType(fun1Class, tryResult, typeOf[Unit]), typeOf[() => Unit])
        (customParents ::: funParents).map(TypeTree(_))
      }

      val stateVar =
        ValDef(Modifiers(Flags.MUTABLE | Flags.PRIVATE | Flags.LOCAL), nme.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))

      val resultVal =
        ValDef(NoMods, nme.result, TypeTree(futureSystemOps.promType(resultType)), futureSystemOps.createProm[Nothing](WeakTypeTag(resultType)).tree)

      val execContextVal =
        ValDef(NoMods, nme.execContext, TypeTree(execContext.tpe), execContext)

      val apply0Def =
        DefDef(NoMods, nme.apply, Nil, List(Nil), TypeTree(definitions.UnitTpe), Apply(Ident(nme.apply), Literal(Constant(null)) :: Nil))

      val applyFSM: DefDef = {
        val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), nme.tr, TypeTree(tryResult), EmptyTree)))
        DefDef(NoMods, nme.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), asyncBody).updateAttachment(ChangeOwnerAttachment(originalOwner))
      }

      atPos(asyncBody.pos)(ClassDef(NoMods, tpnme.stateMachine, Nil,
                                     gen.mkTemplate(parents, noSelfType, NoMods, List(Nil),
                                                     List(stateVar, resultVal, execContextVal, apply0Def, applyFSM))))
    }

    Block(List(stateMachine,
                ValDef(NoMods, nme.stateMachine, TypeTree(), Apply(Select(New(Ident(tpnme.stateMachine)), nme.CONSTRUCTOR), Nil)),
                spawn(Ident(nme.stateMachine), Select(Ident(nme.stateMachine), nme.execContext))),
           promiseToFuture(Select(Ident(nme.stateMachine), nme.result), resultType))
  }
}

// This was originally a macro -- TODO: complete integration with compiler universe (use global instead of scala.reflect.internal stuff)
abstract class AsyncTransform(val asyncBase: AsyncBase) extends AnfTransform with AsyncAnalysis with Lifter with LiveVariables {
  import u._
  import typingTransformers.{TypingTransformApi, typingTransform}

  // synthesize the state machine logic -- explode the apply method's rhs and lift local vals to field defs in the state machine
  def asyncTransform(body: Tree, applySym: Symbol, trParamSym: Symbol, asyncPos: Position)(resultType: Type): Either[Tree, (Tree, List[Tree])] = {
    val stateMachineClass = applySym.owner

    markContainsAwait(body) // TODO AM: is this needed?
    reportUnsupportedAwaits(body)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree0: Block = anfTransform(body, applySym)

    val anfTree = futureSystemOps.postAnfTransform(anfTree0)

    cleanupContainsAwaitAttachments(anfTree)
    markContainsAwait(anfTree)

    val asyncBlock = buildAsyncBlock(anfTree, SymLookup(stateMachineClass, trParamSym))

    // generate lean code for the simple case of `async { 1 + 1 }`
    if (asyncBlock.asyncStates.lengthCompare(1) == 0) Left(spawn(body, Select(Ident(nme.stateMachine), nme.execContext)))
    else {
      val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

      // live variables analysis
      // the result map indicates in which states a given field should be nulled out
      val assignsOf = fieldsToNullOut(asyncBlock.asyncStates, liftedFields)

      for ((state, flds) <- assignsOf) {
        val assigns = flds.map { fld =>
          val fieldSym = fld.symbol
          val assign = Assign(gen.mkAttributedStableRef(thisType(fieldSym.owner), fieldSym), mkZero(fieldSym.info, asyncPos))
          val nulled = nullOut(fieldSym)
          if (isLiteralUnit(nulled)) assign
          else Block(nulled :: Nil, assign)
        }
        val asyncState = asyncBlock.asyncStates.find(_.state == state).get
        asyncState.stats = assigns ++ asyncState.stats
      }

      val liftedSyms = liftedFields.map(_.symbol).toSet
      liftedSyms.foreach { sym =>
        if (sym != null) {
          sym.owner = stateMachineClass
          if (sym.isModule)
            sym.asModule.moduleClass.owner = stateMachineClass
        }
      }
      // Replace the ValDefs in the splicee with Assigns to the corresponding lifted
      // fields. Similarly, replace references to them with references to the field.
      //
      // This transform will only be run on the RHS of `def foo`.
      val useFields: (Tree, TypingTransformApi) => Tree = (tree, api) => tree match {
        case _ if api.currentOwner == stateMachineClass      =>
          api.default(tree)
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) =>
          api.atOwner(api.currentOwner) {
            val fieldSym = tree.symbol
            if (fieldSym.asTerm.isLazy) literalUnit
            else {
              val lhs = atPos(tree.pos) {
                gen.mkAttributedStableRef(thisType(fieldSym.owner.asClass), fieldSym)
              }
              assignUnitType(treeCopy.Assign(tree, lhs, api.recur(rhs))).changeOwner((fieldSym, api.currentOwner))
            }
          }
        case _: DefTree if liftedSyms(tree.symbol)           =>
          EmptyTree
        case Ident(name) if liftedSyms(tree.symbol)          =>
          val fieldSym = tree.symbol
          atPos(tree.pos) {
            gen.mkAttributedStableRef(thisType(fieldSym.owner.asClass), fieldSym).setType(tree.tpe)
          }
        case _                                               =>
          api.default(tree)
      }

      val liftablesUseFields = liftedFields.map {
        case vd: ValDef if !vd.symbol.asTerm.isLazy => vd
        case x                                      => typingTransform(x, stateMachineClass)(useFields)
      }

      liftablesUseFields.foreach { t =>
        if (t.symbol != null) {
          stateMachineClass.info.decls.enter(t.symbol)
          // TODO AM: refine the resetting of the lazy flag -- this is so that local lazy vals that are lifted to the class
          // actually get their LazyRef allocated to the var that holds the lazy val's reference
          t.symbol.resetFlag(Flags.LAZY)
        }
      }

      val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler(WeakTypeTag(transformType(resultType))))
      val applyRhs = typingTransform(applyBody, stateMachineClass)(useFields)

      if (AsyncUtils.verbose) {
        val location = try body.pos.source.path catch {
          case _: UnsupportedOperationException => body.pos.toString
        }
        logDiagnostics(location, anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
      }
      futureSystemOps.dot(applySym, body).foreach(f => f(asyncBlock.toDot))

      Right((cleanupContainsAwaitAttachments(applyRhs), liftablesUseFields))
    }
  }

  def logDiagnostics(location: String, anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
    AsyncUtils.vprintln("===== DOT =====")
    AsyncUtils.vprintln(block.toDot)
  }
}
