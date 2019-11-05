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

package scala.collection.immutable

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import scala.io.Source

/**
  * Generate line charts for Vector benchmarks.
  *
  * Run benchmark and collect raw data:
  *   bench/jmh:run -rff vector-bench.csv -jvmArgs "-Xms256M -Xmx1G" scala.collection.immutable.VectorBenchmark2
  *
  * Generate diagram data:
  *   bench/runMain scala.collection.immutable.GenerateVectorBenchmark2Charts test/benchmarks/vector-bench.csv test/benchmarks/vector-bench-data.js
  */
object GenerateVectorBenchmark2Charts extends App {
  case class Result(name: String, score: Double, error: Double, size: Int)
  val data = Source.fromFile(args(0), "UTF8").getLines().drop(1).map { s =>
    val a = s.split(',')
    def unquote(s: String): String = s.substring(1, s.length-1)
    def local(s: String): String = {
      val i = s.lastIndexOf('.')
      if(i < 0) s else s.substring(i+1)
    }
    Result(local(unquote(a(0))), a(4).toDouble, a(5).toDouble, a(7).toInt)
  }.toIndexedSeq.groupBy(_.name)

  def printChartData(out: PrintWriter, name: String, rss: IndexedSeq[Result]*): Unit = {
    println(s"""drawChart(new Options("$name"), benchmarkData.$name);""")
    val sizes = rss.flatten.map(_.size).toSet.toIndexedSeq.sorted
    val bySize = rss.map(_.iterator.map(r => (r.size, r)).toMap)
    out.println(s"  $name: [")
    var first = true
    sizes.foreach { size =>
      if(!first) out.println(",")
      else first = false
      val line = bySize.map(_.get(size).map(r => Seq(r.score, r.score-r.error, r.score+r.error)).getOrElse(Seq(null, null, null))).flatten
      out.print(s"    [$size, ${line.mkString(", ")}]")
    }
    out.println()
    out.print("  ]")
  }

  val baseNames = data.keySet.filter(_.startsWith("nv")).map(_.drop(2)).toSeq.sorted
  val comparisons = baseNames.map { s =>
    data.get("v"+s).map(v => (s, data("nv"+s), v))
  }.flatten

  val out = new PrintWriter(new BufferedWriter(new FileWriter(args(1))))
  out.println("var benchmarkData = {")

  var first = true
  for((baseName, nvRes, vRes) <- comparisons) {
    if(!first) out.println(",")
    else first = false
    printChartData(out, baseName, vRes, nvRes)
  }

  out.println()
  out.println("};")
  out.close()
}
