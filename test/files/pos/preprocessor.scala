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

//@if(feature == "baz")
//import bla

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

//@if(feature == "baz")
//package object blah
