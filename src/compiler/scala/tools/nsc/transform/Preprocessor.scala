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

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Preprocessor transformations
  */
abstract class Preprocessor extends Transform {
  import global._
  import definitions._

  val phaseName: String = "preprocessor"

  def newTransformer(unit: CompilationUnit): Transformer =
    new PreprocessorTransformer(unit)

  class PreprocessorTransformer(unit: CompilationUnit) extends Transformer {
    override def transform(tree: Tree): Tree = {
      val tree1 = super.transform(tree)
      tree1 match {
        case t @ ClassDef(Conditions(mods, conds), _, _, _) =>
          if(evalBoolean(conds)) t.copy(mods = mods) else EmptyTree
        case t @ DefDef(Conditions(mods, conds), _, _, _, _, _) =>
          if(evalBoolean(conds)) t.copy(mods = mods) else EmptyTree
        case t @ ValDef(Conditions(mods, conds), _, _, _) =>
          if(evalBoolean(conds)) t.copy(mods = mods) else EmptyTree
        case t @ TypeDef(Conditions(mods, conds), _, _, _) =>
          if(evalBoolean(conds)) t.copy(mods = mods) else EmptyTree
        case t @ ModuleDef(Conditions(mods, conds), _, _) =>
          if(evalBoolean(conds)) t.copy(mods = mods) else EmptyTree
        case Annotated(Condition(cond), arg) =>
          if(evalBoolean(cond)) arg else EmptyTree
        case t => t
      }
    }

    object Condition {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case a @ Apply(Select(New(SingletonTypeTree(Literal(Constant("<if>")))), nme.CONSTRUCTOR), args) =>
          if(args.length != 1) reporter.error(a.pos, "one argument expected in preprocessor @if annotation")
          Some(args.head)
        case _ => None
      }
    }

    object Conditions {
      def unapply(mods: Modifiers): Option[(Modifiers, List[Tree])] = {
        val (cs, as) = mods.annotations.partition {
          case a @ Apply(Select(New(SingletonTypeTree(Literal(Constant("<if>")))), nme.CONSTRUCTOR), args) =>
            if(args.length != 1) reporter.error(a.pos, "one argument expected in preprocessor @if annotation")
            true
          case _ => false
        }
        if(cs.isEmpty) None
        else Some((mods.mapAnnotations(_ => as), cs.map { case Apply(Select(New(SingletonTypeTree(Literal(Constant("<if>")))), nme.CONSTRUCTOR), args) => args.head }))
      }
    }

    lazy val config: Map[String, Set[String]] = {
      val raw = global.settings.preprocessorConfig.unparse.map(_.substring(2))
      val split = raw.flatMap { s =>
        val i = s.indexOf('=')
        if(i == -1) Nil
        else List((s.substring(0, i), s.substring(i+1)))
      }
      val c = split.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
      log("Configuration: "+c)
      c
    }

    def configBoolean(name: String): Boolean = {
      val p = config.get(name).getOrElse(Set.empty).nonEmpty
      log(s"configBoolean($name) = $p")
      p
    }

    def configBoolean(name: String, value: String): Boolean = {
      val p = config.get(name).getOrElse(Set.empty).contains(value)
      log(s"configBoolean($name, $value) = $p")
      p
    }

    def evalBoolean(trees: List[Tree]): Boolean = trees.forall(evalBoolean)

    def evalBoolean(tree: Tree): Boolean = tree match {
      case Ident(TermName(name)) =>
        configBoolean(name)
      case Apply(Select(Ident(TermName(name)), TermName("$eq$eq")), List(Literal(Constant(value: String)))) =>
        configBoolean(name, value)
      case Apply(Select(lhs, TermName("$amp$amp")), List(rhs)) =>
        evalBoolean(lhs) && evalBoolean(rhs)
      case Apply(Select(lhs, TermName("$bar$bar")), List(rhs)) =>
        evalBoolean(lhs) || evalBoolean(rhs)
      case Select(t, TermName("unary_$bang")) =>
        !evalBoolean(t)
      case _ =>
        reporter.error(tree.pos, "unsupported expression in preprocessor predicate:\n  "+showRaw(tree))
        false
    }
  }
}
