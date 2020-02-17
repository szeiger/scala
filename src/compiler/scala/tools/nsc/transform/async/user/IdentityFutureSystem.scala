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
package user

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.api
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.whitebox.Context
import scala.tools.nsc.Global

object AsyncId {
  @scala.async.asyncMethod("scala.tools.nsc.transform.async.user.IdentityFutureSystem")
  def async[T](body: T): T = macro ???

  @compileTimeOnly("`await` must be enclosed in an `async` block")
  @scala.async.awaitMethod("scala.tools.nsc.transform.async.user.IdentityFutureSystem")
  def await[T](awaitable: T): T = ???
}


/*
object AsyncTestLVFutureSystem extends IdentityFutureSystemBase {

  var log: List[(String, Any)] = Nil
  def assertNulledOut(a: Any): Unit = assert(log.exists(_._2 == a), log)
  def assertNotNulledOut(a: Any): Unit = assert(!log.exists(_._2 == a), log)
  def clear(): Unit = log = Nil

  def apply(name: String, v: Any): Unit =
    log ::= (name -> v)

  override def mkOps(u: SymbolTable, isPastErasure: Boolean = false): Ops[u.type] = new LVOps[u.type](u, isPastErasure)
  class LVOps[Universe <: SymbolTable](u0: Universe, isPastErasure: Boolean) extends IdentityOps[Universe](u0, isPastErasure) {
    override def nullOut(name: u.Expr[String], v: u.Expr[Any]): u.Expr[Unit] =
      u.reify { AsyncTestLVFutureSystem(name.splice, v.splice) }
  }
}
*/


/**
  * A trivial implementation of [[FutureSystem]] that performs computations
  * on the current thread. Useful for testing.
  */
class Box[A] {
  var a: A = _
}
class IdentityFutureSystemBase extends FutureSystem {

  type Prom[A] = Box[A]
  type Fut[A] = A
  type ExecContext = Unit
  type Tryy[A] = scala.util.Try[A]


  def mkOps(u: Global): Ops[u.type] = new IdentityOps[u.type](u)

  class IdentityOps[Universe <: Global](u0: Universe) extends Ops[Universe](u0) {
    import u._

    lazy val Try_class: Symbol       = rootMirror.requiredClass[scala.util.Try[_]]
    lazy val Box_class: Symbol       = rootMirror.requiredClass[scala.tools.nsc.transform.async.user.Box[_]]
    lazy val Success_class: Symbol   = rootMirror.requiredClass[scala.util.Success[_]]
    lazy val Failure_class: Symbol   = rootMirror.requiredClass[scala.util.Failure[_]]
    lazy val Function1_class: Symbol = rootMirror.requiredClass[scala.Function1[_, _]]
    lazy val Function1_apply: Symbol = Function1_class.info.member(TermName("apply"))
    lazy val Try_get: Symbol         = Try_class.info.member(TermName("get"))
    lazy val Try_isFailure: Symbol   = Try_class.info.member(TermName("isFailure"))
    lazy val Box_setA: Symbol        = Box_class.info.member(TermName("a_$eq"))

    def promType(tp: Type): Type = appliedType(Box_class, tp)
    def tryType(tp: Type): Type = appliedType(Try_class, tp)
    def tryTypeToResult(tp: Type): Type = tp.baseType(Try_class).typeArgs.headOption.getOrElse(NoType)

    def createProm[A](resultType: Type): Expr[Prom[A]] =
      Apply(Select(New(TypeTree(promType(resultType))), nme.CONSTRUCTOR), Nil)

    // called during typer
    def promiseToFuture[A](prom: Expr[Prom[A]]): Expr[Fut[A]] = Select(prom, newTermName("a"))

    def future(a: Tree, execContext: Tree): Tree = a

    override def defaultExecContext: Tree = literalUnitExpr

    def futureUnit(execContext: Tree): Tree = literalUnitExpr

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[Tryy[A] => B],
                         execContext: Expr[ExecContext], aTp: Type): Expr[Unit] = {
      val sel = Select(fun, Function1_apply)
      Apply(sel, tryySuccess(future, aTp) :: Nil)
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit] = {
      val valueGet = Apply(Select(value, Try_get), Nil)
      Apply(Select(prom, Box_setA), valueGet :: Nil)
    }

    def mkAttributedSelectApplyIfNeeded(qual: Tree, sym: Symbol) = {
      val sel = gen.mkAttributedSelect(qual, sym)
      if (isPastErasure) Apply(sel, Nil) else sel
    }

    def tryyIsFailure[A](tryy: Expr[scala.util.Try[A]]): Expr[Boolean] = {
      mkAttributedSelectApplyIfNeeded(tryy, Try_isFailure)
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      mkAttributedSelectApplyIfNeeded(tryy, Try_get)
    }

    def tryySuccess[A](a: Expr[A], aTp: Type): Expr[Tryy[A]] = {
      if(isPastErasure)
        New(Success_class, a)
      else
        New(appliedType(Success_class, aTp :: Nil), a)

      //if(isPastErasure) New(Success_class, a)
      //else Apply(Select(New(TypeTree(appliedType(Success_class, WildcardType))), nme.CONSTRUCTOR), a :: Nil)

      //Apply(TypeApply(Select(New(Success_class), nme.CONSTRUCTOR), TypeTree(definitions.UnitTpe) :: Nil), a :: Nil)

      //if(isPastErasure) New(Success_class, a)
      //else Apply(Select(New(Success_class), nme.CONSTRUCTOR), a :: Nil)
    }

    def tryyFailure[A](a: Expr[Throwable]): Expr[Tryy[A]] = {
      assert(isPastErasure)
      New(Failure_class, a)
    }
  }
}

object IdentityFutureSystem extends IdentityFutureSystemBase
