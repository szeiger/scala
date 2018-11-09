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

package scala.reflect.reify

import scala.reflect.macros.contexts.Context

abstract class Source {
  val c: Context

  import c.universe._
  import definitions._

  def materializeFile: Tree = Literal(Constant(c.enclosingPosition.source.path))

  def materializeAbsFile: Tree = { //TODO all I get is a relative file name
    val f = c.enclosingPosition.source.file.file
    Literal(Constant(if(f eq null) null else f.getAbsolutePath))
  }

  def materializeLine: Tree = Literal(Constant(c.enclosingPosition.line))

  def materializeColumn: Tree = Literal(Constant(c.enclosingPosition.column))
}
