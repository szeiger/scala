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

package scala.collection
package immutable

import scala.annotation.switch
import scala.collection.mutable.{ArrayBuilder, Builder, ReusableBuilder}
import scala.collection.generic.DefaultSerializable
import java.util.Arrays

import scala.reflect.ClassTag

object NVector extends StrictOptimizedSeqFactory[NVector] {
  import NVectorStatics._

  def empty[A]: NVector[A] = NVector0

  def from[E](it: collection.IterableOnce[E]): NVector[E] =
    it match {
      case v: NVector[E] => v
      case _ =>
        val knownSize = it.knownSize
        if (knownSize == 0) empty[E]
        else if (knownSize > 0 && knownSize <= WIDTH) {
          val a1 = new Array[Any](knownSize)
          it.iterator.copyToArray(a1)
          new NVector1[E](a1)
        } else {
          (newBuilder ++= it).result()
        }
    }

  def newBuilder[A]: ReusableBuilder[A, NVector[A]] = new NVectorBuilder[A]
}

/** Base class for vectors of different depths (0 to 6).
  *
  * Data structures are different for each level. Arrays are always left-aligned and truncated.
  */
sealed abstract class NVector[+A]
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, NVector, NVector[A]]
    with StrictOptimizedSeqOps[A, NVector, NVector[A]]
    with IterableFactoryDefaults[A, NVector]
    with DefaultSerializable { self =>
  import NVectorStatics._

  override def iterableFactory: SeqFactory[NVector] = NVector

  //override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): NVector[B] = ???

  private[collection] def validate(): Unit
}

private final class NVectorBuilder[A] extends ReusableBuilder[A, NVector[A]] {
  import NVectorStatics._

  private[this] var a6: Array[Array[Array[Array[Array[Array[Any]]]]]] = _
  private[this] var a5: Array[Array[Array[Array[Array[Any]]]]] = _
  private[this] var a4: Array[Array[Array[Array[Any]]]] = _
  private[this] var a3: Array[Array[Array[Any]]] = _
  private[this] var a2: Array[Array[Any]] = _
  private[this] var a1: Array[Any] = _
  private[this] var len: Int = 0
  private[this] var depth: Int = 0

  def clear(): Unit = {
    a6 = null
    a5 = null
    a4 = null
    a3 = null
    a2 = null
    a1 = null
    len = 0
    depth = 0
  }

  def addOne(elem: A): this.type = {
    val i1 = len & MASK
    if(i1 == 0) advance()
    a1(i1) = elem
    len += 1
    this
  }

  private[this] def advance(): Unit = (depth: @switch) match {
    case 0 =>
      a1 = new Array[Any](WIDTH)
      depth = 1
    case 1 =>
      if(len == WIDTH) {
        a2 = new Array[Array[Any]](WIDTH)
        a2(0) = a1
        a1 = new Array[Any](WIDTH)
        a2(1) = a1
        depth = 2
      }
    case 2 =>
      if(len == WIDTH2) {
        a3 = new Array[Array[Array[Any]]](WIDTH)
        a3(0) = a2
        a2 = new Array[Array[Any]](WIDTH)
        a3(1) = a2
        a2(0) = a1
        a1 = new Array[Any](WIDTH)
        a2(1) = a1
        depth = 3
      } else {
        val i1 = len & MASK
        val i2 = len >> BITS
        if(i1 == 0) {
          a1 = new Array[Any](WIDTH)
          a2(i2) = a1
        }
      }
    case 3 =>
      if(len == WIDTH3) {
        depth = 4
        ???
      } else {
        val i1 = len & MASK
        val i2 = (len >> BITS) & MASK
        val i3 = len >> BITS2
        if(i1 == 0) {
          if(i2 == 0) {
            a2 = new Array[Array[Any]](WIDTH)
            a3(i3) = a2
          }
          a1 = new Array[Any](WIDTH)
          a2(i2) = a1
        }
      }
    case _ => ???
  }

  def result(): NVector[A] = {
    if(len == 0) NVector.empty
    else if(len <= WIDTH) {
      if(len == WIDTH) new NVector1(a1)
      else new NVector1(copyOf(a1, len))
    } else if(len <= WIDTH2) {
      //new NVector2(a2, 0, len)
      if(len == WIDTH2)
        new NVector2(a2, 0, len)
      else {
        val i1 = (len-1) & MASK
        val i2 = (len-1) >> BITS
        val data = Arrays.copyOf(a2, i2+1)
        if(i1+1 != WIDTH)
          data(data.length-1) = copyOf(data(data.length-1), i1+1)
        new NVector2(data, 0, len)
      }
    } else if(len <= WIDTH3) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >> BITS) & MASK
      val i3 = ((len-1) >> BITS2)
      val data = Arrays.copyOfRange(a3, 1, i3)
      val prefix2 = Arrays.copyOfRange(a3(0), 1, WIDTH)
      val prefix1 = a3(0)(0)
      val suffix2 = Arrays.copyOf(a3(i3), i2)
      val suffix1 = copyIfDifferentSize(a3(i3)(i2), i1+1)
      new NVector3(prefix1, prefix2, data, suffix2, suffix1, 0, len)
    } else ???
  }

  private[collection] def printState(): Unit = {
    println("Length: "+len)
    if(a6 ne null) logArray(a6, "  ", "a6: ")
    if(a5 ne null) logArray(a5, "  ", "a5: ")
    if(a4 ne null) logArray(a4, "  ", "a4: ")
    if(a3 ne null) logArray(a3, "  ", "a3: ")
    if(a2 ne null) logArray(a2, "  ", "a2: ")
    if(a1 ne null) logArray(a1.asInstanceOf[Array[AnyRef]], "  ", "a1: ")
  }
}

/** Empty vector */
private final object NVector0 extends NVector[Nothing] {
  def length = 0
  def apply(index: Int) = throw new IndexOutOfBoundsException

  override def updated[B >: Nothing](index: Int, elem: B): NVector[B] = throw new IndexOutOfBoundsException

  override def appended[B >: Nothing](value: B): NVector[B] = new NVector1(Array[Any](value))

  override def prepended[B >: Nothing](value: B): NVector[B] = new NVector1(Array[Any](value))

  override def appendedAll[B >: Nothing](suffix: collection.IterableOnce[B]): NVector[B] =
    NVector.from(suffix)

  private[collection] def validate(): Unit = ()
}

/** Flat ArraySeq-like structure.
  *
  * @param data The vector's content, with length between 1 and WIDTH.
  */
private final class NVector1[+A](val data: Array[Any]) extends NVector[A] {
  import NVectorStatics._

  @inline def length = data.length

  @inline def apply(index: Int): A = data(index).asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val a = data.clone()
    a(index) = elem
    new NVector1(a)
  }

  override def appended[B >: A](value: B): NVector[B] = {
    if(length < WIDTH) {
      val a = copyOf(data, length+1)
      a(length) = value
      new NVector1(a)
    } else new NVector2(Array[Array[Any]](data, Array[Any](value)), 0, length+1)
  }

  override def prepended[B >: A](value: B): NVector[B] = {
    if(length < WIDTH) new NVector1(copyPrepend(value, data))
    else new NVector2(Array[Array[Any]](Array[Any](value), data), WIDTH-1, length+1)
  }

  private[collection] def validate(): Unit = {
    assert(length > 0 && length <= WIDTH, s"length is $length, should be > 0 and <= ${WIDTH}")
  }
}

/** Simple 2-level radix tree with offset (for amortized O(1) prepend).
  *
  * @param data The radix tree with the content in the 2nd level. Length of all arrays is between
  *             1 and WIDTH (i.e. for offsets != 0 the switch to Vector3 already happens at
  *             offset + length > WIDTH, not at length > WIDTH.
  * @param offset The position of the first element in the tree, must be < WIDTH. Note that all arrays
  *               are left-aligned, i.e. the offset does not apply to the first sub-array.
  * @param length The actual number of elements in the vector
  */
private final class NVector2[+A](val data: Array[Array[Any]], offset: Int, val length: Int) extends NVector[A] {
  import NVectorStatics._

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val io = index + offset
    val i2 = io >> BITS
    var i1 = io & MASK
    if(i2 == 0) i1 -= offset
    data(i2)(i1).asInstanceOf[A]
  }

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val io = index + offset
    val i2 = io >> BITS
    var i1 = io & MASK
    if(i2 == 0) i1 -= offset
    val a2 = data.clone()
    val a1 = a2(i2).clone()
    a1(i1) = elem
    a2(i2) = a1
    new NVector2(a2, offset, length)
  }

  override def appended[B >: A](value: B): NVector[B] = {
    val io = length + offset
    if(io < WIDTH2) {
      var a1 = data(data.length-1)
      if(a1.length < WIDTH) {
        a1 = copyOf(a1, a1.length+1)
        a1(a1.length-1) = value
        val a2 = data.clone()
        a2(a2.length-1) = a1
        new NVector2(a2, offset, length+1)
      } else {
        val a2 = Arrays.copyOf(data, data.length+1)
        a2(a2.length-1) = Array[Any](value)
        new NVector2(a2, offset, length+1)
      }
    } else {
      val prefix1 = data(0)
      ???
    }
  }

  override def prepended[B >: A](value: B): NVector[B] = {
    if(offset > 0) {
      val a2 = Arrays.copyOf[Array[Any]](data, data.length)
      val data1 = a2(0)

      /*
      val a1 = new Array[Any](data1.length+1)
      System.arraycopy(data1, 0, a1, 1, data1.length)
      a1(0) = value
      */

      val a1 = copyPrepend(value, data1)

      a2(0) = a1
      new NVector2(a2, offset-1, length+1)
    } else if(length < WIDTH2) {

      /*
      val a2 = new Array[Array[Any]](data.length+1)
      System.arraycopy(data, 0, a2, 1, data.length)
      a2(0) = Array[Any](value)
      */

      val a2 = copyPrepend(Array[Any](value), data)

      new NVector2(a2, WIDTH-1, length+1)
    } else ???
  }

  private[collection] def validate(): Unit = {
    assert(length > WIDTH && length <= WIDTH2, s"length is $length, should be > ${WIDTH} and <= ${WIDTH2}")
    assert(data.length > 0 && data.length <= WIDTH, s"data.length is ${data.length}, should be > 0 and <= ${WIDTH}")
    assert(data.forall(_ ne null), "data should not contain null entries")
    assert(data.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all arrays should be > 0 and <= ${WIDTH}")
    val sum = data.map(_.length).sum
    assert(sum == length, s"sum of data lengths ($sum) should be vector length ($length)")
    assert(data(0).length + offset == WIDTH, s"data(0).length (${data(0).length}) + offset ($offset) should be ${WIDTH}")
  }
}

/** 3-level radix tree with fingers at both ends. Max size is WIDTH for prefix1 and suffix2, WIDTH-1 for
  * prefix2 and suffix1, and WIDTH-2 for data.
  *
  * @param data The main data, excluding prefix and suffix.
  * @param offset The offset in the prefix, must be < WIDTH2; the main data is always full on the left
  * @param length The actual number of elements in the vector
  * @param
  */
private final class NVector3[+A](prefix1: Array[Any], prefix2: Array[Array[Any]],
                                 data: Array[Array[Array[Any]]],
                                 suffix2: Array[Array[Any]], suffix1: Array[Any],
                                 offset: Int, val length: Int) extends NVector[A] {
  import NVectorStatics._

  private[collection] def validate(): Unit = {
    assert(length > WIDTH2 && length <= WIDTH3, s"length is $length, should be > ${WIDTH2} and <= ${WIDTH3}")
    assert(data.length <= (WIDTH-2), s"data.length is ${data.length}, should be <= ${WIDTH-2}")
    assert(data.forall(_ ne null), "data should not contain null entries")
    assert(data.forall(_.forall(_ ne null)), "data children should not contain null entries")
    assert(prefix2.forall(_ ne null), "prefix2 should not contain null entries")
    assert(suffix2.forall(_ ne null), "suffix2 should not contain null entries")
    assert(data.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all data children should be > 0 and <= ${WIDTH}")
    assert(data.forall(_.forall(a => a.length > 0 && a.length <= WIDTH)), s"length of all data grandchildren should be > 0 and <= ${WIDTH}")
    assert(prefix2.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all prefix2 children should be > 0 and <= ${WIDTH}")
    assert(suffix2.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all suffix2 children should be > 0 and <= ${WIDTH}")
    val prefixlength = prefix2.map(_.length).sum + prefix1.length
    val sum = data.map(_.map(_.length).sum).sum + suffix2.map(_.length).sum + prefixlength + suffix1.length
    assert(sum == length, s"sum of data lengths ($sum) should be vector length ($length)")
    assert(prefixlength + offset == WIDTH2, s"prefixlength ($prefixlength) + offset ($offset) should be ${WIDTH2}")
  }

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val io = index + offset
    val i3 = io >> BITS2
    if(i3 == 0) { // prefix
      val i2 = (index >> BITS) & MASK
      val i1 = index & MASK
      if(i2 == 0) prefix1(i1)
      else prefix2(i2-1)(i1)
    } else if(i3 <= data.length) { // data
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      data(i3-1)(i2)(i1)
    } else { // suffix
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i2 == suffix2.length) suffix1(i1)
      else suffix2(i2)(i1)
    }
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val io = index + offset
    val i3 = io >> BITS2
    if(i3 == 0) { // prefix
      val i2 = (index >> BITS) & MASK
      val i1 = index & MASK
      if(i2 == 0) {
        val p1 = prefix1.clone()
        p1(i1) = elem
        new NVector3(p1, prefix2, data, suffix2, suffix1, offset, length)
      } else {
        val p2 = prefix2.clone()
        p2(i2-1) = p2(i2-1).clone()
        p2(i2-1)(i1) = elem
        new NVector3(prefix1, p2, data, suffix2, suffix1, offset, length)
      }
    } else if(i3 <= data.length) { // data
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      data(i3-1)(i2)(i1)
      val d3 = data.clone()
      val d2 = d3(i3-1).clone()
      d3(i3-1) = d2
      val d1 = d2(i1).clone()
      d2(i1) = d1
      d1(i1) = elem
      new NVector3(prefix1, prefix2, d3, suffix2, suffix1, offset, length)
    } else { // suffix
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i2 == suffix2.length) {
        val s1 = suffix1.clone()
        s1(i1) = elem
        new NVector3(prefix1, prefix2, data, suffix2, s1, offset, length)
      } else {
        val s2 = suffix2.clone()
        s2(i2) = s2(i2).clone()
        s2(i2)(i1) = elem
        new NVector3(prefix1, prefix2, data, s2, suffix1, offset, length)
      }
    }
  }

  /*
  override def appended[B >: A](value: B): NVector[B] = {
    val io = length + offset
    if(io < WIDTH3) {
      val i3 = io >> BITS2
      val i2 = (io >> BITS) & MASK
      var i1 = io & MASK

      if(i2 == 0) i1 -= offset
      val a2 = Arrays.copyOf[Array[Any]](data, i2+1)
      if(i1 == 0) {
        a2(i2) = Array[Any](value)
      } else {
        val a1 = copyOf(a2(i2), i1+1)
        a1(i1) = value
        a2(i2) = a1
      }
      new NVector2(a2, offset, length+1)

    } else ???
  }
  */

  /*
  override def appended[B >: A](value: B): NVector[B] = ???
  override def prepended[B >: A](value: B): NVector[B] = ???
  //override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): NVector[B] = ???
  */
}

/*
private final class NVector3[+A](val data: Array[Any], offset: Int, val length: Int) extends NVector[A] {
  @inline def apply(index: Int): A = ???
  override def updated[B >: A](index: Int, elem: B): NVector[B] = ???
  override def appended[B >: A](value: B): NVector[B] = ???
  override def prepended[B >: A](value: B): NVector[B] = ???
  //override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): NVector[B] = ???
}
*/

private[immutable] object NVectorStatics {
  final val BITS = 5
  final val WIDTH = 1 << BITS
  final val MASK = WIDTH - 1
  final val BITS2 = BITS * 2
  final val WIDTH2 = 1 << BITS2
  final val BITS3 = BITS * 3
  final val WIDTH3 = 1 << BITS3
  final val BITS4 = BITS * 4
  final val WIDTH4 = 1 << BITS4
  final val BITS5 = BITS * 5
  final val WIDTH5 = 1 << BITS5

  @inline
  final def level(len: Int) = ((1 + 6*BITS)-Integer.numberOfLeadingZeros(len-1))/BITS

  @inline
  final def copyOf(a: Array[Any], len: Int): Array[Any] =
    Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], len).asInstanceOf[Array[Any]]

  @inline final def copyAppend(a: Array[Any], elem: Any): Array[Any] = {
    val ac = Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], a.length).asInstanceOf[Array[Any]]
    ac(ac.length-1) = elem
    ac
  }

  @inline
  final def copyPrepend(elem: Any, a: Array[Any]): Array[Any] = {
    val ac = new Array[Any](a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline
  final def copyPrepend(elem: Array[Any], a: Array[Array[Any]]): Array[Array[Any]] = {
    val ac = new Array[Array[Any]](a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline
  final def copyIfDifferentSize(a: Array[Any], len: Int): Array[Any] =
    if(a.length == len) a
    else Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], len).asInstanceOf[Array[Any]]

  final def logArray[T <: AnyRef](a: Array[T], indent: String = "", prefix: String = ""): Unit = {
    def classifier(x: AnyRef): Char =
      if(x eq null) '-'
      else if(x.isInstanceOf[Array[AnyRef]]) 'A'
      else 'o'
    def atos(a: Array[_ <: AnyRef]): String =
      if(a eq null) "-"
      else a.map(classifier).mkString("[", "", "]") + " (" + a.length + ")"
    println(indent + prefix + atos(a))
    var i = 0
    while(i < a.length) {
      if(a(i).isInstanceOf[Array[AnyRef]])
        logArray(a(i).asInstanceOf[Array[AnyRef]], indent + "  ", s"$i. ")
      i += 1
    }
  }
}
