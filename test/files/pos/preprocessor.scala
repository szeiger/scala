package abc.de

import scala.annotation.Annotation

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

@if(feature == "import")
import bla.blub

@if(feature == "packageobject")
package object abc {
  xyz
}

@if(feature == "foo")
package fg {
  class inPackage
}

import abc.de.fg.inPackage

@if(feature == "baz")
class Foo {
  @if(feature == "foo") def foo = x
  bla
}

class Bar {
  @if(feature == "baz")
  type T = S

  @if(feature == "baz")
  val x = y

  @if(feature == "baz")
  var x = y
}

class C1 {
  @if(feature == "baz")
  def f = xxx

  @if(feature == "foo")
  def f = 42

  def g: Int = f
}

class C2 {
  def f = {
    (xxx: @if(feature == "baz"))
    (42: @if(feature == "foo"))
  }

  def g: Int = f
}
