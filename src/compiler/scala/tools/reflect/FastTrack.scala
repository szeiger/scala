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

package scala.tools
package reflect

import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{ Analyzer, Macros }
import scala.reflect.runtime.Macros.currentMirror
import scala.reflect.quasiquotes.{ Quasiquotes => QuasiquoteImpls }
import scala.collection.mutable

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
class FastTrack[MacrosAndAnalyzer <: Macros with Analyzer](val macros: MacrosAndAnalyzer) {

  import macros._
  import global._
  import definitions._
  import scala.language.implicitConversions
  import treeInfo.Applied

  def contains(symbol: Symbol): Boolean = getOrNull(symbol) != null
  def apply(symbol: Symbol): FastTrackEntry = {
    val f = getOrNull(symbol)
    if(f == null) throw new IllegalArgumentException
    f
  }
  def get(symbol: Symbol): Option[FastTrackEntry] = Option(getOrNull(symbol))

  private def getOrNull(symbol: Symbol): FastTrackEntry = fastTrackCache().getOrElse(symbol, {
    val c = annotationFastTrackCache()
    val fo = symbol.annotations.iterator.map(ai => c.getOrElse(ai.symbol, null)).find(_ != null)
    fo match {
      case Some(f) => fastTrackCache().put(symbol, f)
        f
      case _ => null
    }
  })

  private implicit def context2taggers(c0: MacroContext): Taggers { val c: c0.type } =
    new { val c: c0.type = c0 } with Taggers
  private implicit def context2macroimplementations(c0: MacroContext): FormatInterpolator { val c: c0.type } =
    new { val c: c0.type = c0 } with FormatInterpolator
  private implicit def context2quasiquote(c0: MacroContext): QuasiquoteImpls { val c: c0.type } =
    new { val c: c0.type = c0 } with QuasiquoteImpls
  private def makeBlackbox(sym: Symbol)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    sym -> new FastTrackEntry(pf, isBlackbox = true)
  private def makeWhitebox(sym: Symbol)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    sym -> new FastTrackEntry(pf, isBlackbox = false)

  private def makeBlackBoxIfExists(sym_pf: (Symbol, PartialFunction[Applied, MacroContext => Tree])): mutable.Map[Symbol, FastTrackEntry] =
    sym_pf match { case (sym, _) if !sym.exists => mutable.Map.empty case (sym, pf) => mutable.Map(makeBlackbox(sym)(pf))}

  final class FastTrackEntry(pf: PartialFunction[Applied, MacroContext => Tree], val isBlackbox: Boolean) extends (MacroArgs => Any) {
    def validate(tree: Tree) = pf isDefinedAt Applied(tree)
    def apply(margs: MacroArgs): margs.c.Expr[Nothing] = {
      val MacroArgs(c, _) = margs
      // Macros validated that the pf is defined here - and there's not much we could do if it weren't.
      c.Expr[Nothing](pf(Applied(c.expandee))(c))(c.WeakTypeTag.Nothing)
    }
  }

  /** A map from a set of pre-established macro symbols to their implementations. */
  private val fastTrackCache = perRunCaches.newGeneric[mutable.Map[Symbol, FastTrackEntry]] {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    mutable.Map[Symbol, FastTrackEntry](
      makeBlackbox(        materializeClassTag) { case Applied(_, ttag :: Nil, _)                 => _.materializeClassTag(ttag.tpe) },
      makeBlackbox(     materializeWeakTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = false) },
      makeBlackbox(         materializeTypeTag) { case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = true) },
      makeBlackbox(           ApiUniverseReify) { case Applied(_, ttag :: Nil, (expr :: _) :: _)  => c => c.materializeExpr(c.prefix.tree, EmptyTree, expr) },
      makeBlackbox(            StringContext_f) { case _                                          => _.interpolate },
      makeBlackbox(ReflectRuntimeCurrentMirror) { case _                                          => c => currentMirror(c).tree },
      makeWhitebox(  QuasiquoteClass_api_apply) { case _                                          => _.expandQuasiquote },
      makeWhitebox(QuasiquoteClass_api_unapply) { case _                                          => _.expandQuasiquote }
     )
  }

  /** Cache by marker annotation symbols instead of method symbols */
  private val annotationFastTrackCache = perRunCaches.newGeneric[mutable.Map[Symbol, FastTrackEntry]] {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    makeBlackBoxIfExists(global.async.fastTrackAnnotationEntry)
  }
}
