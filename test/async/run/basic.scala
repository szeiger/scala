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

import scala.tools.nsc.transform.async.user.{lateasync, autoawait}

object Test extends App {
  @lateasync
  def test: Any = {
    @autoawait def id(a: String) = a
    id("foo") + id("bar")
  }

  println(test)
}
