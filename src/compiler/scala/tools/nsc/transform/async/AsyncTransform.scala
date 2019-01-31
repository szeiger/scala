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

import user.AsyncBase
import scala.reflect.internal.{Flags, SymbolTable}
import scala.tools.nsc.Global

// This was originally a macro -- TODO: integrate with compiler universe
abstract class AsyncTransform(val asyncBase: AsyncBase) extends AnfTransform with AsyncAnalysis with Lifter with LiveVariables {
  import u._
  import typingTransformers.{TypingTransformApi, typingTransform}

  def asyncTransform(body: Tree, execContext: Tree, enclosingOwner: Symbol, asyncPos: Position)(resultType: Type): Tree = {
    markContainsAwait(body) // TODO AM: is this needed?
    reportUnsupportedAwaits(body)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree0: Block = anfTransform(body, enclosingOwner)

    val anfTree = futureSystemOps.postAnfTransform(anfTree0)

    cleanupContainsAwaitAttachments(anfTree)
    markContainsAwait(anfTree)

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    val resultTypeTag = WeakTypeTag(uncheckedBounds(transformType(resultType)))
    val tryResult = appliedType(weakTypeOf[util.Try[_]], resultType)

    val applyDefDefDummyBody: DefDef = {
      val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), name.tr, TypeTree(tryResult), EmptyTree)))
      DefDef(NoMods, name.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), literalUnit)
    }
    val apply0DefDef: DefDef =
      DefDef(NoMods, name.apply, Nil, List(Nil), TypeTree(definitions.UnitTpe), Apply(Ident(name.apply), literalNull :: Nil))

//    def transformParentTypes(tps: List[Type]) = {
//      val tpsErased = tps.map(transformType)
//      assert(tpsErased.head.typeSymbol.isClass)
//      tpsErased
//    }

    // Create `ClassDef` of state machine with empty method bodies for `resume` and `apply`.
    // TODO AM: can we only create the symbol for the state machine class for now and then type check the assembled whole later,
    // instead of splicing stuff in (spliceMethodBodies)?
    val stateMachine: ClassDef = {
      val body: List[Tree] = {
        val stateVar = ValDef(Modifiers(Flags.MUTABLE | Flags.PRIVATE | Flags.LOCAL), name.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))
        val resultAndAccessors =
          mkMutableField(transformType(futureSystemOps.promType(uncheckedBounds(resultType))), name.result, futureSystemOps.createProm[Nothing](resultTypeTag).tree)
        val execContextValDef =
          mkField(execContext.tpe, name.execContext, execContext)

        List(stateVar) ++ resultAndAccessors ++ execContextValDef ++ List(applyDefDefDummyBody, apply0DefDef)
      }

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

      // TODO AM: after erasure we have to change the order of these parents etc
      val templ = gen.mkTemplate((customParents ::: funParents).map(TypeTree(_)), noSelfType, NoMods, List(Nil), body)

      // TODO AM: can we skip the type checking and just create a symbol?
      val classSym = enclosingOwner.newClassSymbol(name.stateMachineT, asyncPos, 0)
      val cd = ClassDef(NoMods, name.stateMachineT, Nil, templ).setSymbol(classSym)
      enteringExplicitOuter { classSym.setInfo(typingTransformers.callsiteTyper.namer.monoTypeCompleter(cd)) }
      typingTransformers.callsiteTyper.typedClassDef(atPos(asyncPos)(cd)).asInstanceOf[ClassDef]
    }

    val asyncBlock: AsyncBlock = {
      val symLookup = SymLookup(stateMachine.symbol, applyDefDefDummyBody.vparamss.head.head.symbol)
      buildAsyncBlock(anfTree, symLookup)
    }

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

    def startStateMachine: Tree = {
      val stateMachineSpliced: ClassDef =
        spliceMethodBodies(liftedFields, stateMachine, atPos(asyncPos)(asyncBlock.onCompleteHandler(resultTypeTag)), apply0DefDef.symbol, applyDefDefDummyBody.symbol, enclosingOwner)

      val (stateMachineUsingOuter, newStateMachine) = {
          // TODO create state machine class during macro expansion in typers, and only delay actual ANF/Async transform,
          // happening in the state machine's apply method, to minimize the need for deftree manipulation in later phases
          //
          // Since explicit outers has already run (it happens before erasure), we must run it ourselves on the class we're synthesizing.
          // The state machine class is going to be lifted out by the flatten phase, but expressions contained in it
          // will likely still need to access the outer class's instance.
          // Thus, we add the standard outer argument to the constructor and supply it when instantiating the state machine.
          // Lambdalift will also look for this and transform appropriately.

          val explicitOuters = new explicitOuter.ExplicitOuterTransformer(typingTransformers.callsiteTyper.context.unit)
          val stateMachineWithOuters = explicitOuters.transform(stateMachineSpliced)

          val machineWithBridges = stateMachineWithOuters.asInstanceOf[ClassDef] match {
            case ClassDef(mods, names, tparams, impl@Template(parents, self, stats)) =>
              val unit = typingTransformers.callsiteTyper.context.unit
              val (bridges, toBeRemoved) =
                enteringErasure{new erasure.GenerateBridges(unit, stateMachineWithOuters.symbol).generate()}

              treeCopy.ClassDef(stateMachineWithOuters, mods, names, tparams,
                                 treeCopy.Template(impl, parents, self, stats ::: bridges.map { tree => typingTransformers.callsiteTyper.typed(tree) }))
          }

          val applyCtor = enteringExplicitOuter { typingTransformers.callsiteTyper.typed(Apply(Select(New(Ident(stateMachine.symbol)), nme.CONSTRUCTOR), Nil)) }
          val newStateMachine = explicitOuters.atOwner(stateMachine.symbol.owner) {
            explicitOuters.transform(applyCtor)
          }

          (machineWithBridges, newStateMachine)
        }

      def selectStateMachine(selection: TermName) = Select(Ident(name.stateMachine), selection)
      def selectStateMachineResult =
        applyNilAfterUncurry(selectStateMachine(name.result))

      Block(List[Tree](
        stateMachineUsingOuter,
        ValDef(NoMods, name.stateMachine, TypeTree(), newStateMachine),
        spawn(Ident(name.stateMachine), selectStateMachine(name.execContext))),
        promiseToFuture(selectStateMachineResult, resultType))
    }

    val isSimple = asyncBlock.asyncStates.size == 1
    val result =
      if (isSimple) spawn(body, execContext) // generate lean code for the simple case of `async { 1 + 1 }`
      else startStateMachine

    if(AsyncUtils.verbose) {
      val location = try body.pos.source.path catch { case _: UnsupportedOperationException => body.pos.toString }
      logDiagnostics(location, anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
    }
    futureSystemOps.dot(enclosingOwner, body).foreach(f => f(asyncBlock.toDot))
    cleanupContainsAwaitAttachments(result)
  }

  def logDiagnostics(location: String, anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
    AsyncUtils.vprintln("===== DOT =====")
    AsyncUtils.vprintln(block.toDot)
  }

  /**
   *  Build final `ClassDef` tree of state machine class.
   *
   *  @param  liftables  trees of definitions that are lifted to fields of the state machine class
   *  @param  tree       `ClassDef` tree of the state machine class
   *  @param  applyBody  tree of onComplete handler (`apply` method)
   *  @return            transformed `ClassDef` tree of the state machine class
   */
  def spliceMethodBodies(liftables: List[Tree], tree: ClassDef, applyBody: Tree, apply0Sym: Symbol, apply1Sym: Symbol, enclosingOwner: Symbol): ClassDef = {
    val liftedSyms = liftables.map(_.symbol).toSet
    val stateMachineClass = tree.symbol
    liftedSyms.foreach {
      sym =>
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
      case _ if api.currentOwner == stateMachineClass          =>
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

    val liftablesUseFields = liftables.map {
      case vd: ValDef if !vd.symbol.asTerm.isLazy => vd
      case x          => typingTransform(x, stateMachineClass)(useFields)
    }

    tree.children.foreach(_.changeOwner((enclosingOwner, tree.symbol)))
    val treeSubst = tree

    /* Fixes up DefDef: use lifted fields in `body` */
    def fixup(dd: DefDef, body: Tree, api: TypingTransformApi): Tree = {
      val spliceeAnfFixedOwnerSyms = body
      val newRhs = typingTransform(spliceeAnfFixedOwnerSyms, dd.symbol)(useFields)
      val newRhsTyped = api.atOwner(dd, dd.symbol)(api.typecheck(newRhs))
      treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, newRhsTyped)
    }

    liftablesUseFields.foreach(t => if (t.symbol != null) stateMachineClass.info.decls.enter(t.symbol))

    // TODO AM: refine the resetting of the lazy flag -- this is so that local lazy vals that are lifted to the class
    // actually get their LazyRef allocated to the var that holds the lazy val's reference
    if (isPastErasure)
      liftablesUseFields.foreach(t => if (t.symbol != null) t.symbol.resetFlag(Flags.LAZY))

    val bridges = Nil
//      if (isPastErasure) List(apply1BridgeDef(tryAny, apply1Sym), apply0BridgeDef(apply0Sym)).map(dd => typingTransformers.callsiteTyper.typedDefDef(dd.asInstanceOf[typingTransformers.global.DefDef]))
//      else Nil

    val result0 = transformAt(treeSubst) {
      case t@Template(parents, self, stats) =>
        (api: TypingTransformApi) => {
          treeCopy.Template(t, parents, self, liftablesUseFields ++ stats ++ bridges)
        }
    }
    val result = transformAt(result0) {
      case dd@DefDef(_, name.apply, _, List(List(_)), _, _) if dd.symbol.owner == stateMachineClass =>
        (api: TypingTransformApi) =>
          val typedTree = fixup(dd, applyBody.changeOwner((enclosingOwner, dd.symbol)), api)
          typedTree
    }
    result.asInstanceOf[ClassDef]
  }
}
