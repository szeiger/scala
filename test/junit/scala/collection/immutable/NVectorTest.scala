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

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable.{ListBuffer, StringBuilder}
import scala.reflect.{ClassTag, classTag}

@RunWith(classOf[JUnit4])
class NVectorTest {
  import NVectorUtils.validateDebug

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = NVector(0) ++ NVector(1 to 64: _*)

    assertEquals(NVector(0, 1), v take 2)
    assertEquals(NVector(63, 64), v takeRight 2)
    assertEquals(NVector(2 to 64: _*), v drop 2)
    assertEquals(NVector(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(NVector.empty[Int], v drop Int.MaxValue)
    assertEquals(NVector.empty[Int], v dropRight Int.MaxValue)

    assertEquals(NVector.empty[Int], v take Int.MinValue)
    assertEquals(NVector.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }

  @Test
  def hasCorrectPrependedAll(): Unit = {
    val els = NVector(1 to 1000: _*)

    for (i <- 0 until els.size) {
      val (prefix, suffix) = els.splitAt(i)
      validateDebug(prefix)
      validateDebug(suffix)
      validateDebug(prefix ++: suffix)
      assertEquals(els, prefix ++: suffix)
      assertEquals(els, prefix.toList ++: suffix)
    }
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(NVector.empty, NVector.empty)
    assertSame(NVector.empty, NVector())
    val m = NVector("a")
    assertSame(m, NVector.from(m))
    assertSame(m, NVector.apply(m: _*))
  }

  @Test def checkSearch: Unit = SeqTests.checkSearch(NVector(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

  @Test
  def emptyIteratorReuse(): Unit = {
    assertSame(NVector.empty.iterator, NVector.empty.iterator)
    assertSame(NVector.empty.iterator, NVector(1).drop(1).iterator)
  }

  @Test
  def t11122_prependedAll_Iterator(): Unit = {
    val i = Iterator.from(1).take(3)
    assertEquals(NVector(1, 2, 3, 0), NVector(0).prependedAll(i))
  }

  @Test
  def concat: Unit = {
    assertEquals(NVector.from(1 to 100), NVector.from(1 to 7) concat NVector.from(8 to 100))
  }

  @Test
  def copyToArray: Unit = {
    val array = Array.fill(100)(2)
    NVector.fill(100)(1).copyToArray(array, 0, 100)
    assertEquals(array.toSeq, Seq.fill(100)(1))
  }

  @Test
  def vectorIteratorDrop(): Unit = {
    val underlying = NVector(0 to 10010: _*)

    val totalSize = underlying.size

    for (start <- 1056 to 10000) {
      val it = underlying.iterator.drop(start)
      assertEquals(totalSize - start, it.knownSize)
      assertEquals(totalSize - start, it.size)
      assertTrue(it.hasNext)
      assertEquals(start, it.next())
    }
  }
  def intercept[T <: Throwable: ClassTag](fn: => Any): T = {
    try {
      fn
      fail(s"expected a ${classTag[T].runtimeClass.getName} to be thrown")
      ???
    } catch {
      case x: T => x
    }
  }
  @Test
  def vectorIteratorDropToEnd(): Unit = {
    val underlying = NVector(0)

    for (start <- List(1,2,3,4,99)) {
      {
        var it = underlying.iterator.drop(start)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
        it = it.drop(0)
        assertFalse(it.hasNext)
        it = it.drop(1)
        assertFalse(it.hasNext)
        it = it.drop(99)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
      }

      {
        var it = underlying.iterator.drop(start)
        intercept[NoSuchElementException](it.next)
        it = it.drop(0)
        it = it.drop(1)
        it = it.drop(99)
        intercept[NoSuchElementException](it.next)
      }
    }
  }
  @Test
  def vectorIteratorRepeated(): Unit = {
    val underlying = NVector(1 to 10001: _*)


    for (stepSize <- List(0, 1, 2, 3, 4, 8, 10, 24, 32, 63, 64, 100)) {
      var it:Iterator[Int] = underlying.iterator
      for (stepCount <- 1 to 10) {
        it = it.drop(stepSize)
        assertTrue(it.hasNext)
        val expected = (stepSize + 1) * stepCount
        assertEquals(expected, it.next())
      }
    }
  }
  @Test
  def vectorFill(): Unit = {
    var i = 0
    val test = NVector.fill(10){
      i += 1
      i * 10
    }
    assertEquals(List(10,20,30,40,50,60,70,80,90,100), test)
    assertEquals(10, test.length)
    assertEquals(10, test.head)
    assertEquals(10, test(0))
    assertEquals(20, test(1))
    assertEquals(80, test(7))
    assertEquals(100, test(9))

    assertEquals(0, test.indexOf(10))
    assertEquals(8, test.indexOf(90))
    assertEquals(-1, test.indexOf(1000))
  }

  @Test
  def tapEach(): Unit = {
    val lb = ListBuffer[Int]()

    val v =
      NVector(1,2,3)
      .tapEach(lb += _)
      .tapEach(lb += _)

    assertEquals(ListBuffer(1,2,3,1,2,3), lb)
    assertEquals(NVector(1,2,3), v)


    val f: Any => Unit = println

    // test that type info is not lost
    val x: NVector[Char] = NVector[Char]().tapEach(f)
  }

  @Test
  def vectorIteratorTake(): Unit = {
    val v = NVector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 6
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.take($j)", v2.toArray.take(j), v2.iterator.take(j).toArray)
    }
  }

  @Test
  def vectorIteratorDrop2(): Unit = {
    val v = NVector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 60
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.drop($j)", v2.toArray.drop(j), v2.iterator.drop(j).toArray)
    }
  }

  @Test
  def vectorIteratorSlice(): Unit = {
    val v = NVector.from(0 to 50)
    for {
      i <- -100 to 4000 by 40
      j <- -100 to 4000 by 60
      k <- -100 to 4000 by 60
    } {
      val v2 = v.take(i)
      assertArrayEquals(s"<${v2.length}>.slice($j, $k)", v2.toArray.slice(j, k), v2.iterator.slice(j, k).toArray)
    }
  }

  @Test
  def t11600(): Unit = {
    locally {
      abstract class Base
      class Derived1 extends Base
      class Derived2 extends Base
      val d1 = new Derived1
      val d2 = new Derived2

      locally {
        val arraySeq = ArraySeq(d1)
        val vector = NVector(arraySeq: _*)
        assertEquals(arraySeq, ArraySeq(d1)) // ensure arraySeq is not mutated
        assertEquals(vector.updated(0, d2), NVector(d2))
      }

      locally {
        val list = List(d1)
        val vector = NVector.from(list)
        assertEquals(list, vector)
        assertEquals(List(d2), vector.updated(0, d2))
      }
    }

    locally {
      // ensure boxing logic works:
      val arraySeq = ArraySeq(1,2,3,4,5)
      val vector = NVector(arraySeq: _*)

      assertEquals(1 to 5, vector)
      assertEquals(vector.updated(0, 20), NVector(20,2,3,4,5))
      assertEquals(vector.updated(0, ""), NVector("",2,3,4,5))
      assertEquals(1 to 5, arraySeq) // ensure arraySeq is not mutated
    }
    locally {
      // ensure boxing logic works:
      val arr = Array(1)
      val vector = NVector.from(arr)
      assertEquals(arr.toList, vector)
      assertEquals(List(20), vector.updated(0, 20))
      assertEquals(List(""), vector.updated(0, ""))
    }
  }

  def t11636(): Unit = {
    val a: NVector[String] = "O" +: Iterator.continually("E").take(2101).foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val a0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(2101) ++ ArraySeq("C")

    val b: NVector[String] = "O" +: Iterator.continually("E").take(223) .foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val b0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(223) ++ ArraySeq("C")

    val c: NVector[String] = "O" +: Iterator.continually("E").take(135) .foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val c0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(135) ++ ArraySeq("C")

    val d: NVector[String] = "O" +: Iterator.continually("E").take(0)   .foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val d0: ArraySeq[String] = ArraySeq("O", "C")

    val e: NVector[String] = "O" +: Iterator.continually("E").take(376) .foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val e0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(376) ++ ArraySeq("C")

    val f: NVector[String] = "O" +: Iterator.continually("E").take(365) .foldLeft(NVector.empty[String])((v, e) => v :+ e) :+ "C"
    val f0: ArraySeq[String] = ArraySeq("O") ++ Iterator.continually("E").take(365) ++ ArraySeq("C")

    assertEquals(a0 ++ b0, a ++ b)
    assertEquals(a0 ++ b0 ++ c0, a ++ b ++ c)
    assertEquals(a0 ++ b0 ++ c0 ++ d0, a ++ b ++ c ++ d)
    assertEquals(a0 ++ b0 ++ c0 ++ d0 ++ e0, a ++ b ++ c ++ d ++ e)
    assertEquals(a0 ++ b0 ++ c0 ++ d0 ++ e0 ++ f0, a ++ b ++ c ++ d ++ e ++ f)
  }




  val WIDTH = 32
  val allSizes = Seq(0, 1, WIDTH, WIDTH+1, WIDTH*WIDTH, WIDTH*WIDTH+1, WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH+1,
    WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH+1, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH+1,
    WIDTH*WIDTH*WIDTH*WIDTH*WIDTH*WIDTH, WIDTH*WIDTH*WIDTH*WIDTH*WIDTH*WIDTH+1, Int.MaxValue)
  val smallSizes = allSizes.filter(_ <= WIDTH*WIDTH*WIDTH*WIDTH)
  val verySmallSizes = allSizes.filter(_ <= WIDTH*WIDTH)

  @Test
  def testAligned: Unit = for(size <- smallSizes) {
    val v = NVector.range(0, size)
    //println(v.toDebugString)
    validateDebug(v)
    assertEquals(size, v.length)
    assertEquals(0 until size, v)
  }

  @Test
  def testNonAligned: Unit = for(size <- smallSizes.filter(n => n > 0 && n < WIDTH*WIDTH*WIDTH*WIDTH)) {
    val v0 = NVector.range(1, size)
    val v = 0 +: v0
    //println(v0.toDebugString)
    //println(v.toDebugString)
    validateDebug(v)
    assertEquals(size, v.length)
    assertEquals(0 until size, v)
  }


  @Test
  def testUpdate: Unit = for(size <- smallSizes) {
    var v = NVector.range(0, size)
    var i = 0
    while(i < size) {
      v = v.updated(i, i-13)
      i += 1
    }
    assertEquals(-13 until size-13, v)
  }

  @Test
  def testSlice: Unit = for(size <- smallSizes) {
    val step = size/16 max 1
    val v = NVector.range(0, size)
    for {
      from <- 0 until size by step
      to <- from until size by step
    } {
      val v2 = v.slice(from, to)
      validateDebug(v2)
      assertEquals(from until to, v2)
    }
  }

  @Test
  def testSlice2: Unit = for(size <- Seq(10, 100, 1000, 10000)) {
    val o = new AnyRef
    var coll = NVector.fill(size)(o)
    val inc = size / 10
    if(inc > 0) {
      var i = 0
      while(i < size) {
        var j = i + inc
        while(j < size) {
          //println(s"--- $i, $j")
          //println(NVectorUtils.toDebugString(coll))
          coll.slice(i, j)
          j += inc
        }
        i += inc
      }
    }
  }

  @Test
  def testTail: Unit = for(size <- verySmallSizes) {
    var i = 0
    var v = NVector.range(0, size)
    //println("testTail: "+size)
    while(!v.isEmpty) {
      v = v.tail
      i += 1
      assertEquals(i until size, v)
    }
  }

  @Test
  def testRebuild: Unit = for(size <- smallSizes) {
    for(prependSize <- verySmallSizes) {
      var v = NVector.range(0, size + prependSize)
      for(i <- prependSize to 0 by -1) {
        v = i +: v
      }
      validateDebug(v)
      val b = NVector.newBuilder[Int]
      b.addAll(v)
      val v2 = b.result()
      validateDebug(v2)
      assertEquals(v, v2)
    }
  }

  @Test
  def testAppendAll: Unit = for(size <- smallSizes) {
    for(appendSize <- smallSizes) {
      val v = NVector.range(0, size)
      val v2 = NVector.range(size, size + appendSize)
      val v3 = v.appendedAll(v2)
      assertEquals(0 until (size + appendSize), v3)
    }
  }
}

object NVectorUtils {
  import NVectorStatics._

  def validateDebug(v: NVector[_]): Unit = {
    try validate(v) catch {
      case ex: Throwable =>
        throw new RuntimeException("Validation failed: " + ex.getMessage + "\n" + toDebugString(v), ex)
    }
  }

  def validate(v: NVector[_]): Unit = {
    val count = v.vectorSliceCount
    val len = (0 until count).map(v.vectorSlicePrefixLength _)
    val alen = (0 until count).map { i =>
      if(i == 0 || i == (count-1)) v.vectorSlice(i).length
      else validateArrays(v.vectorSlice(i).asInstanceOf[Array[Array[AnyRef]]], sliceName(v, i))
    }
    val running = alen.scanLeft(0)(_ + _).tail
    running.zip(len).zipWithIndex.foreach { case ((r, p), i) =>
      assert(r == p, s"sum of slice lengths ($r) at ${sliceName(v, i)} should be equal to prefix length ($p)")
    }
  }

  def sliceName(v: NVector[_], i: Int): String = {
    val count = v.vectorSliceCount
    val level = (count+1)/2
    if(i+1 == level) "data"
    else if(i+1 < level) "prefix" + (i+1)
    else "suffix" + (count-i)
  }

  def toDebugString(v: NVector[_]): String = {
    val sb = new StringBuilder()
    val level = (v.vectorSliceCount+1)/2
    val len = (0 until v.vectorSliceCount).map(v.vectorSlicePrefixLength _)
    sb.append(s"NVector$level(lenghts=[${len.mkString(", ")}])\n")
    for(i <- 0 until v.vectorSliceCount)
      logArray(sb, v.vectorSlice(i), "  ", s"${sliceName(v, i)}: ")
    sb.result()
  }

  def toDebugString(v: NVectorSliceBuilder): String = {
    val sb = new StringBuilder()
    sb.append(s"$v\n")
    logArray(sb, v.getSlices, "  ", "slices: ")
    sb.result()
  }

  private def toDebugString(v: NVectorBuilder[_]): String = {
    val sb = new StringBuilder()
    sb.append(s"$v\n")
    val d = v.getData.asInstanceOf[Array[Array[AnyRef]]]
    logArray(sb, d(5), "  ", "a6: ")
    logArray(sb, d(4), "  ", "a5: ", d(5).asInstanceOf[Array[Array[AnyRef]]], "a6")
    logArray(sb, d(3), "  ", "a4: ", d(4).asInstanceOf[Array[Array[AnyRef]]], "a5")
    logArray(sb, d(2), "  ", "a3: ", d(3).asInstanceOf[Array[Array[AnyRef]]], "a4")
    logArray(sb, d(1), "  ", "a2: ", d(2).asInstanceOf[Array[Array[AnyRef]]], "a3")
    logArray(sb, d(0), "  ", "a1: ", d(1).asInstanceOf[Array[Array[AnyRef]]], "a2")
    sb.result()
  }

  private[this] def validateArrays[T](a: Array[Array[T]], name: String): Int = {
    var i = 0
    var total = 0
    while(i < a.length) {
      assert(a(i) ne null, s"$name($i) should not be null")
      assert(a(i).length == WIDTH, s"$name($i) should have length $WIDTH, has ${a(i).length}")
      if(a(i).isInstanceOf[Array[Array[AnyRef]]])
        total += validateArrays(a(i).asInstanceOf[Array[Array[AnyRef]]], s"$name($i)")
      else if(a(i).isInstanceOf[Array[AnyRef]])
        total += a(i).asInstanceOf[Array[AnyRef]].length
      i += 1
    }
    total
  }

  private[this] def logArray[T <: AnyRef](sb: StringBuilder, a: Array[T], indent: String = "", prefix: String = "", findIn: Array[Array[T]] = null, findInName: String = "<array>"): StringBuilder = {
    def classifier(x: AnyRef): String =
      if(x eq null) "-"
      else if(x.isInstanceOf[Array[AnyRef]]) "A"
      else if((x: Any).isInstanceOf[Int]) x.toString
      else "o"
    def atos(a: Array[_ <: AnyRef]): String =
      if(a eq null) "-"
      else {
        var i = 0
        var startNum: Option[Int] = None
        var currentNum = 0
        val b = new StringBuilder().append("[")
        while(i < a.length) {
          if(i != 0) b.append(",")
          (a(i): Any) match {
            case n: Int =>
              if(i == 0) {
                startNum = Some(n)
                currentNum = n
              } else if(startNum.isDefined) {
                if(n == currentNum +1) currentNum = n
                else startNum = None
              }
            case _ => startNum = None
          }
          b.append(classifier(a(i)))
          i += 1
        }
        if(startNum.isDefined && startNum.get != currentNum) {
          b.clear()
          b.append("[").append(startNum.get).append("...").append(currentNum)
        }
        b.append("]").toString + " (" + a.length + ")"
      }
    if(a eq null)
      sb.append(indent + prefix + "-\n")
    else {
      val idx = Option(findIn).map(_.indexWhere(_ eq a)).getOrElse(-1)
      if(idx >= 0) {
        sb.append(s"$indent$prefix= $findInName($idx)\n")
      } else {
        sb.append(indent + prefix + atos(a) + "\n")
        var i = 0
        while(i < a.length) {
          if(a(i).isInstanceOf[Array[AnyRef]]) {
            logArray(sb, a(i).asInstanceOf[Array[AnyRef]], indent + "  ", s"$i. ")
          }
          i += 1
        }
      }
    }
    sb
  }
}
