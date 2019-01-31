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

import scala.tools.nsc.Mode
import scala.tools.nsc.transform.{Transform, TypingTransformers}

abstract class AsyncPhase extends Transform with TypingTransformers  {
  private val asyncNames_ = new AsyncNames[global.type](global)
  import global._

  val phaseName: String = "async"
  override def enabled = true // TODO: should be off by default, enabled by flag
//  {
//    (currentRun.runDefinitions match {
//      case null => new definitions.RunDefinitions
//      case rd => rd
//    }).Async_async.exists
//  }

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  private lazy val autoAwaitSym = symbolOf[user.autoawait]
  private lazy val autoAsyncSym = symbolOf[user.async]

  // TODO: support more than the original late expansion tests
  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    abstract class AsyncTransformBase(asyncBase: user.AsyncBase) extends AsyncTransform(asyncBase) {
      val u: global.type = global
      import u._

      val asyncNames: AsyncNames[u.type] = asyncNames_

      def typecheck(tree: Tree): Tree = localTyper.typed(tree)
      def abort(pos: Position, msg: String): Nothing = {localTyper.context.reporter.error(pos, msg); ???}
      def error(pos: Position, msg: String): Unit = localTyper.context.reporter.error(pos, msg)

      val typingTransformers =
        new TypingTransformers {
          def callsiteTyper = localTyper
        }
    }
    object asyncTransformerId extends AsyncTransformBase(user.AsyncId) {
      private val asyncIDModule = rootMirror.getRequiredModule("scala.tools.nsc.transform.async.user.AsyncId")
      val Async_async = definitions.getMember(asyncIDModule, nme.async)
      val Async_await = definitions.getMember(asyncIDModule, nme.await)
    }

    object asyncTransformerConcurrent extends AsyncTransformBase(user.ScalaConcurrentAsync) {
      val Async_async = currentRun.runDefinitions.Async_async
      val Async_await = currentRun.runDefinitions.Async_await
    }

    // TODO AM: does this rely on `futureSystem.Fut[T] = T` (as is the case for the identity future system)
    def transformAutoAwait(awaitable: Tree) =
      localTyper.typedPos(awaitable.pos, Mode.EXPRmode, awaitable.tpe) {
        Apply(gen.mkAttributedRef(asyncTransformerId.Async_await), awaitable :: Nil)
      }

    def transformAutoAsync(rhs: Tree)=
      localTyper.typedPos(rhs.pos, Mode.EXPRmode, rhs.tpe) {
        asyncTransformerId.asyncTransform(rhs, asyncTransformerId.literalUnit, localTyper.context.owner, rhs.pos.makeTransparent)(rhs.tpe)
      }

    def transformAsyncStd(rhs: Tree, execContext: Tree)= {
      val pt = typeOf[scala.concurrent.Future[_]].typeConstructor // no need to apply to rhs.tpe, since we're past erasure
//      println(s"transformAsyncStd $rhs under $pt")
      localTyper.typedPos(rhs.pos, Mode.EXPRmode, pt) {
        asyncTransformerConcurrent.asyncTransform(rhs, execContext, localTyper.context.owner, rhs.pos.makeTransparent)(pt)
      }
    }

    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        case ap@Apply(fun, _) if fun.symbol.hasAnnotation(autoAwaitSym)               => transformAutoAwait(ap)
        case ap@Apply(fun, rhs :: execContext :: Nil) if fun.symbol == asyncTransformerConcurrent.Async_async => transformAsyncStd(rhs, execContext)
        case dd: DefDef if dd.symbol.hasAnnotation(autoAsyncSym)                      => atOwner(dd.symbol) { deriveDefDef(dd) { transformAutoAsync } }
        case vd: ValDef if vd.symbol.hasAnnotation(autoAsyncSym)                      => atOwner(vd.symbol) { deriveValDef(vd) { transformAutoAsync } }
        case tree                                                                     => tree
      }

  }
}
