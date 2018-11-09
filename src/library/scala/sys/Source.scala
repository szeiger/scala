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

package scala.sys

import scala.language.experimental.macros

object Source {
  /** Expands to the name of the source file in which it is called */
  def file: String = macro ???

  /** Expands to the absolute name of the source file in which it is called */
  def absFile: String = macro ???

  /** Expands to the line in the source file in which it is called */
  def line: Int = macro ???

  /** Expands to the column in the source file in which it is called */
  def column: Int = macro ???
}
