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

object AsyncId {
  @scala.async.asyncMethod("scala.tools.nsc.transform.async.user.IdentityFutureSystem")
  def async[T](body: T): T = macro ???

  @compileTimeOnly("`await` must be enclosed in an `async` block")
  @scala.async.awaitMethod("scala.tools.nsc.transform.async.user.IdentityFutureSystem")
  def await[T](awaitable: T): T = ???
}

// Methods with this annotation are translated to having the RHS wrapped in `AsyncId.async { <original RHS> }`
@annotation.meta.field
final class async extends annotation.StaticAnnotation

// Calls to methods with this annotation are translated to `AsyncId.await(<call>)`
@annotation.meta.getter
final class autoawait extends annotation.StaticAnnotation

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


  def mkOps(u: SymbolTable, isPastErasure: Boolean = false): Ops[u.type] = new IdentityOps[u.type](u, isPastErasure)
  class IdentityOps[Universe <: SymbolTable](u0: Universe, isPastErasure: Boolean) extends Ops[Universe](u0, isPastErasure) {
    import u._

    lazy val TryClass = rootMirror.requiredClass[scala.util.Try[_]]
    lazy val Box      = rootMirror.requiredClass[scala.tools.nsc.transform.async.user.Box[_]]

    def promType(tp: Type): Type = appliedType(Box, tp)
    def tryType(tp: Type): Type = appliedType(TryClass, tp)
    def tryTypeToResult(tp: Type): Type = tp.baseType(TryClass).typeArgs.headOption.getOrElse(NoType)

    def createProm(resultType: Type): Tree = Apply(Select(New(TypeTree(promType(resultType))), nme.CONSTRUCTOR), Nil)

    // called during typer
    def promiseToFuture(prom: Tree) = Select(prom, newTermName("a"))

    def future(a: Tree, execContext: Tree): Tree = a

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[Tryy[A] => B],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      fun.splice.apply(util.Success(future.splice))
      literalUnitExpr.splice
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit] = {
      val valueGet = reify { value.splice.get }
      reify {
        prom.splice.a = { if (isPastErasure) Expr[A](Apply(valueGet.tree, Nil)) else valueGet }.splice
        literalUnitExpr.splice
      }
    }

    def tryyIsFailure[A](tryy: Expr[Tryy[A]]): Expr[Boolean] = reify {
      tryy.splice.isFailure
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      val expr = reify { tryy.splice.get }
      if (isPastErasure) Expr[A](Apply(expr.tree, Nil))
      else expr
    }

    def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Success[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match {
          // drop type apply
          case ap@Apply(TypeApply(succ, _), args) => treeCopy.Apply(ap, succ, args)
        })
      else expr
    }
    def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[Tryy[A]] = {
      val expr = reify { scala.util.Failure[A](a.splice) }
      if (isPastErasure)
        Expr[Tryy[A]](expr.tree match {
          // drop type apply
          case ap@Apply(TypeApply(fail, _), args) => treeCopy.Apply(ap, fail, args)
        })
      else expr
    }
  }
}

object IdentityFutureSystem extends IdentityFutureSystemBase
