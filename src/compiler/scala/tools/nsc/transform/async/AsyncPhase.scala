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

import scala.tools.nsc.transform.async.user.AsyncBase
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
  private lazy val lateAsyncSym = symbolOf[user.lateasync]

  private val asyncBase: AsyncBase = user.AsyncId

  // TODO: support more than the original late expansion tests
  class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    object asyncTransformer extends AsyncTransform(asyncBase, global) {
      override val u: global.type = global
      import u._

      val asyncNames: AsyncNames[u.type] = asyncNames_

      private val asyncIDModule = rootMirror.getRequiredModule("scala.tools.nsc.transform.async.user.AsyncId")
      val Async_async = definitions.getMember(asyncIDModule, nme.async)
      val Async_await = definitions.getMember(asyncIDModule, nme.await)

      def typecheck(tree: Tree): Tree = localTyper.typed(tree)
      def abort(pos: Position, msg: String): Nothing = {localTyper.context.reporter.error(pos, msg); ???}
      def error(pos: Position, msg: String): Unit = localTyper.context.reporter.error(pos, msg)

      val typingTransformers =
        new TypingTransformers {
          def callsiteTyper = localTyper.asInstanceOf[global.analyzer.Typer]
        }
    }

    def isAutoAwait(fun: Tree) = fun.symbol.hasAnnotation(autoAwaitSym)
    // TODO AM: does this rely on `futureSystem.Fut[T] = T` (as is the case for the identity future system)
    def transformAwait(awaitable: Tree) =
      localTyper.typed(atPos(awaitable.pos)(Apply(gen.mkAttributedRef(asyncTransformer.Async_await), awaitable :: Nil)), pt = awaitable.tpe)

    def isLateAsync(dd: ValOrDefDef) = dd.symbol.hasAnnotation(lateAsyncSym)
    def transformAsync(rhs: Tree)=
      localTyper.typed(atPos(rhs.pos)(asyncTransformer.asyncTransform(rhs, asyncTransformer.literalUnit, localTyper.context.owner, rhs.pos.makeTransparent)(rhs.tpe)), pt = rhs.tpe)

    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        case ap@Apply(fun, _) if isAutoAwait(fun) => localTyper.typed(transformAwait(ap))
        case dd: DefDef if isLateAsync(dd)        => atOwner(dd.symbol) { deriveDefDef(dd) { transformAsync } }
        case vd: ValDef if isLateAsync(vd)        => atOwner(vd.symbol) { deriveValDef(vd) { transformAsync } }
        case tree                                 => tree
      }

  }
}
