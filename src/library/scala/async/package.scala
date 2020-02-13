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

package scala

package object async {

  final class asyncMethod(futureSystem: String) extends scala.annotation.StaticAnnotation {}

  final class awaitMethod(futureSystem: String) extends scala.annotation.StaticAnnotation {}
}
