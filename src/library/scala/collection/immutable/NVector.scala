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
import java.lang.Math.{max => mmax, min => mmin}

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

  /** Create a Vector with the same element at each index.
    *
    * Unlike `fill`, which takes a by-name argument for the value and can thereby
    * compute different values for each index, this method guarantees that all
    * elements are identical. This allows sparse allocation in O(log n) time and space.
    */
  def fillSparse[A](n: Int)(elem: A): NVector[A] = {
    if(n <= 0) NVector0
    else {
      val b = new NVectorBuilder[A]
      b.initSparse(n, elem)
      b.result()
    }
  }
}


/** Vector is a general-purpose, immutable data structure.  It provides random access and updates
  * in effectively constant time, as well as very fast append and prepend.  Because vectors strike
  * a good balance between fast random selections and fast random functional updates, they are
  * currently the default implementation of immutable indexed sequences.
  *
  * Vectors are implemented by radix-balanced finger trees of width 32. There is a separate subclass
  * for each level (0 to 6, with 0 being the empty vector and 6 a tree with a maximum width of 64).
  *
  * Tree balancing:
  * - Only the first dimension of an array may have a size < WIDTH
  * - prefix1 and suffix1 are never empty
  * - Balancing does not cross the main data array (i.e. prepending never touches the suffix and appending never touches
  *   the prefix). The level is increased/decreased when the affected side plus main data is already full/empty
  * - All arrays are left-aligned and truncated
  */
sealed abstract class NVector[+A]
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, NVector, NVector[A]]
    with StrictOptimizedSeqOps[A, NVector, NVector[A]]
    with IterableFactoryDefaults[A, NVector]
    with DefaultSerializable {
  import NVectorStatics._

  override def iterableFactory: SeqFactory[NVector] = NVector

  //override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): NVector[B] = ???

  private[collection] def validate(): Unit

  private[collection] def validateDebug(): Unit = {
    try validate() catch {
      case ex: Exception =>
        throw new RuntimeException("Validation failed: " + ex.getMessage + "\n" + toDebugString, ex)
    }
  }

  private[collection] def toDebugString: String

  override def className = "NVector"

  /*
  @inline override final def take(n: Int): NVector[A] = slice(0, n)
  @inline override final def drop(n: Int): NVector[A] = slice(n, length)
  @inline override final def takeRight(n: Int): NVector[A] = slice(length - mmax(n, 0), length)
  @inline override final def dropRight(n: Int): NVector[A] = slice(0, length - mmax(n, 0))
  @inline override def tail: NVector[A] = slice(1, length)
  @inline override def init: NVector[A] = slice(0, length-1)

  //override def slice(from: Int, until: Int): NVector[A]

  override def slice(from: Int, until: Int): NVector[A] = {
    val lo = mmax(from, 0)
    val hi = mmin(until, length)
    if (hi > lo) {
      ???
    } else NVector0
  }*/
}

/** Empty vector */
private final object NVector0 extends NVector[Nothing] {
  def length = 0
  def apply(index: Int) = throw new IndexOutOfBoundsException

  override def updated[B >: Nothing](index: Int, elem: B): NVector[B] = throw new IndexOutOfBoundsException

  override def appended[B >: Nothing](elem: B): NVector[B] = new NVector1(Array[Any](elem))

  override def prepended[B >: Nothing](elem: B): NVector[B] = new NVector1(Array[Any](elem))

  override def appendedAll[B >: Nothing](suffix: collection.IterableOnce[B]): NVector[B] =
    NVector.from(suffix)

  override def iterator: Iterator[Nothing] = Iterator.empty

  override def foreach[U](f: Nothing => U): Unit = ()

  private[collection] def validate(): Unit = ()

  private[collection] def toDebugString: String = "NVector0\n"

  override def map[B](f: Nothing => B): NVector[B] = this

  override def tail: NVector[Nothing] = throw new UnsupportedOperationException("empty.tail")

  override def init: NVector[Nothing] = throw new UnsupportedOperationException("empty.init")

  override def slice(from: Int, until: Int): NVector[Nothing] = this
}

/** Flat ArraySeq-like structure.
  *
  * @param data1 The vector's content, with length between 1 and WIDTH.
  */
private final class NVector1[+A](data1: Array[Any]) extends NVector[A] {
  import NVectorStatics._

  @inline def length = data1.length

  @inline def apply(index: Int): A = data1(index).asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    new NVector1(copyUpdate(data1, index, elem))
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) {
      val a = copyOf(data1, length+1)
      a(length) = elem
      new NVector1(a)
    } else new NVector2(data1, WIDTH, empty2, Array[Any](elem), WIDTH+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) new NVector1(copyPrepend1(elem, data1))
    else new NVector2(Array[Any](elem), 1, empty2, data1, length+1)
  }

  override def iterator: Iterator[A] = new ArrayOps.ArrayIterator(data1).asInstanceOf[Iterator[A]]

  override def foreach[U](f: A => U): Unit = foreachElem(data1, f)

  private[collection] def validate(): Unit = {
    assert(length > 0 && length <= WIDTH, s"length is $length, should be > 0 and <= ${WIDTH}")
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector1\n")
    logArray(sb, data1.asInstanceOf[Array[AnyRef]], "  ", "data1: ")
    sb.result()
  }

  override def map[B](f: A => B): NVector[B] = new NVector1(mapElems1(data1, f))

  override def head: A = data1(0).asInstanceOf[A]

  override def last: A = data1(data1.length-1).asInstanceOf[A]

  override def slice(from: Int, until: Int): NVector[A] = {
    val lo = mmax(from, 0)
    val hi = mmin(until, length)
    val newlen = hi - lo
    if(newlen == length) this
    else if(newlen > 0) new NVector1(Arrays.copyOfRange(data1.asInstanceOf[Array[AnyRef]], lo, hi).asInstanceOf[Array[Any]])
    else NVector0
  }
}

/** 2-level radix tree with fingers at both ends.
  *
  * @param prefix1 The level 1 prefix
  * @param len1 The length of prefix1
  * @param data2 The main data, excluding prefix and suffix.
  * @param suffix1 The level 1 suffix
  * @param length The actual number of elements in the vector
  */
private final class NVector2[+A](prefix1: Array[Any], len1: Int,
                                 data2: Array[Array[Any]],
                                 suffix1: Array[Any],
                                 val length: Int) extends NVector[A] {
  import NVectorStatics._

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len1) {
      val io = index - len1
      val i2 = io >> BITS
      val i1 = io & MASK
      if(i2 < data2.length) data2(i2)(i1)
      else suffix1(i1)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len1) {
      val io = index - len1
      val i2 = io >> BITS
      val i1 = io & MASK
      if(i2 < data2.length)
        new NVector2(prefix1, len1, copyUpdate(data2, i2, i1, elem), suffix1, length)
      else
        new NVector2(prefix1, len1, data2, copyUpdate(suffix1, i1, elem), length)
    } else {
      new NVector2(copyUpdate(prefix1, index, elem), len1, data2, suffix1, length)
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(suffix1.length < WIDTH)
      new NVector2(prefix1, len1, data2, copyAppend1(suffix1, elem), length+1)
    else if(data2.length < WIDTH-2)
      new NVector2(prefix1, len1, copyAppend(data2, suffix1), Array[Any](elem), length+1)
    else
      new NVector3(prefix1, len1, data2, WIDTH*(WIDTH-2) + len1, empty3, Array[Array[Any]](suffix1), Array[Any](elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector2(copyPrepend1(elem, prefix1), len1+1, data2, suffix1, length+1)
    else if(data2.length < WIDTH-2)
      new NVector2(Array[Any](elem), 1, copyPrepend(prefix1, data2), suffix1, length+1)
    else
      new NVector3(Array[Any](elem), 1, Array[Array[Any]](prefix1), len1+1, empty3, data2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(data2, f)
    foreachElem(suffix1, f)
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector2(len1=$len1, length=$length)\n")
    logArray(sb, prefix1.asInstanceOf[Array[AnyRef]], "  ", "prefix1: ")
    logArray(sb, data2, "  ", "data2: ")
    logArray(sb, suffix1.asInstanceOf[Array[AnyRef]], "  ", "suffix1: ")
    sb.result()
  }

  private[collection] def validate(): Unit = {
    assert(data2.length <= (WIDTH-2), s"data2.length is ${data2.length}, should be <= ${WIDTH-2}")
    val d2l = validateArrays(data2, "data2")
    assert(len1 == prefix1.length, s"len1 ($len1) should be prefix1.length (${prefix1.length})")
    assert(len1 + d2l + suffix1.length == length, s"sum of lengths ($len1 + $d2l+ ${suffix1.length}) should be vector length ($length)")
  }

  override def map[B](f: A => B): NVector[B] =
    new NVector2(mapElems1(prefix1, f), len1, mapElems(2, data2, f), mapElems1(suffix1, f), length)

  override def head: A = prefix1(0).asInstanceOf[A]

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]
}

/** 3-level radix tree with fingers at both ends. Max size is WIDTH for prefix1 and suffix1, WIDTH-1 for
  * prefix2 and suffix2, and WIDTH-2 for data3.
  *
  * @param prefix1 The level 1 prefix
  * @param len1 The length of prefix1
  * @param prefix2 The level 2 prefix
  * @param len12 The combined length of prefix 1 and all prefix2 subarrays
  * @param data3 The main data, excluding prefix and suffix.
  * @param suffix2 The level 2 suffix
  * @param suffix1 The level 1 suffix
  * @param length The actual number of elements in the vector
  */
private final class NVector3[+A](prefix1: Array[Any], len1: Int,
                                 prefix2: Array[Array[Any]], len12: Int,
                                 data3: Array[Array[Array[Any]]],
                                 suffix2: Array[Array[Any]], suffix1: Array[Any],
                                 val length: Int) extends NVector[A] {
  import NVectorStatics._

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len12) {
      val io = index - len12
      val i3 = io >> BITS2
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i3 < data3.length) data3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len12) {
      val io = index - len12
      val i3 = io >> BITS2
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i3 < data3.length)
        new NVector3(prefix1, len1, prefix2, len12, copyUpdate(data3, i3, i2, i1, elem), suffix2, suffix1, length)
      else if(i2 < suffix2.length)
        new NVector3(prefix1, len1, prefix2, len12, data3, copyUpdate(suffix2, i2, i1, elem), suffix1, length)
      else
        new NVector3(prefix1, len1, prefix2, len12, data3, suffix2, copyUpdate(suffix1, i1, elem), length)
    } else if(index >= len1) {
      val io = index - len1
      new NVector3(prefix1, len1, copyUpdate(prefix2, io >> BITS, io & MASK, elem), len12, data3, suffix2, suffix1, length)
    } else {
      new NVector3(copyUpdate(prefix1, index, elem), len1, prefix2, len12, data3, suffix2, suffix1, length)
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(suffix1.length < WIDTH)
      new NVector3(prefix1, len1, prefix2, len12, data3, suffix2, copyAppend1(suffix1, elem), length+1)
    else if(suffix2.length < WIDTH-1)
      new NVector3(prefix1, len1, prefix2, len12, data3, copyAppend(suffix2, suffix1), Array[Any](elem), length+1)
    else if(data3.length < WIDTH-2)
      new NVector3(prefix1, len1, prefix2, len12, copyAppend(data3, copyAppend(suffix2, suffix1)), empty2, Array[Any](elem), length+1)
    else
      new NVector4(prefix1, len1, prefix2, len12, data3, (WIDTH-2)*WIDTH2 + len12, empty4, Array[Array[Array[Any]]](copyAppend(suffix2, suffix1)), empty2, Array[Any](elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector3(copyPrepend1(elem, prefix1), len1+1, prefix2, len12+1, data3, suffix2, suffix1, length+1)
    else if(len12 < WIDTH2)
      new NVector3(Array[Any](elem), 1, copyPrepend(prefix1, prefix2), len12+1, data3, suffix2, suffix1, length+1)
    else if(data3.length < WIDTH-2)
      new NVector3(Array[Any](elem), 1, empty2, len1, copyPrepend(copyPrepend(prefix1, prefix2), data3), suffix2, suffix1, length+1)
    else
      new NVector4(Array[Any](elem), 1, empty2, 1, Array[Array[Array[Any]]](copyPrepend(prefix1, prefix2)), len12+1, empty4, data3, suffix2, suffix1, length+1)
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(data3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector3(len1=$len1, len12=$len12, length=$length)\n")
    logArray(sb, prefix1.asInstanceOf[Array[AnyRef]], "  ", "prefix1: ")
    logArray(sb, prefix2, "  ", "prefix2: ")
    logArray(sb, data3, "  ", "data3: ")
    logArray(sb, suffix2, "  ", "suffix2: ")
    logArray(sb, suffix1.asInstanceOf[Array[AnyRef]], "  ", "suffix1: ")
    sb.result()
  }

  private[collection] def validate(): Unit = {
    assert(data3.length <= (WIDTH-2), s"data3.length is ${data3.length}, should be <= ${WIDTH-2}")
    val p2l = validateArrays(prefix2, "prefix2")
    val d3l = validateArrays(data3, "data3")
    val s2l = validateArrays(suffix2, "suffix2")
    assert(len1 == prefix1.length, s"len1 ($len1) should be prefix1.length (${prefix1.length})")
    assert(len12 == len1 + p2l, s"len12 ($len12) should be len1 + prefix2 length ($p2l)")
    assert(len12 + d3l + s2l + suffix1.length == length, s"sum of lengths ($len12 + $d3l+ $s2l + ${suffix1.length}) should be vector length ($length)")
  }

  override def map[B](f: A => B): NVector[B] =
    new NVector3(mapElems1(prefix1, f), len1, mapElems(2, prefix2, f), len12, mapElems(3, data3, f), mapElems(2, suffix2, f), mapElems1(suffix1, f), length)

  override def head: A = prefix1(0).asInstanceOf[A]

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]
}

/** 4-level radix tree with fingers at both ends.
  *
  * @param prefix1 The level 1 prefix
  * @param len1 The length of prefix1
  * @param prefix2 The level 2 prefix
  * @param len12 The combined element count of prefix 1 and 2
  * @param prefix3 The level 3 prefix
  * @param len12 The combined element count of prefix 1, 2 and 3
  * @param data4 The main data, excluding prefix and suffix.
  * @param suffix3 The level 2 suffix
  * @param suffix2 The level 2 suffix
  * @param suffix1 The level 1 suffix
  * @param length The actual number of elements in the vector
  */
private final class NVector4[+A](prefix1: Array[Any], len1: Int,
                                 prefix2: Array[Array[Any]], len12: Int,
                                 prefix3: Array[Array[Array[Any]]], len123: Int,
                                 data4: Array[Array[Array[Array[Any]]]],
                                 suffix3: Array[Array[Array[Any]]], suffix2: Array[Array[Any]], suffix1: Array[Any],
                                 val length: Int) extends NVector[A] {
  import NVectorStatics._

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len123) {
      val io = index - len123
      val i4 = io >> BITS3
      val i3 = (io >> BITS2) & MASK
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i4 < data4.length) data4(i4)(i3)(i2)(i1)
      else if(i3 < suffix3.length) suffix3(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len12) {
      val io = index - len12
      prefix3(io >> BITS2)((io >> BITS) & MASK)(io & MASK)
    } else if(index >= len1) {
      val io = index - len1
      prefix2(io >> BITS)(io & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len123) {
      val io = index - len123
      val i4 = io >> BITS3
      val i3 = (io >> BITS2) & MASK
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i4 < data4.length)
        new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, copyUpdate(data4, i4, i3, i2, i1, elem), suffix3, suffix2, suffix1, length)
      else if(i3 < suffix3.length)
        new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, copyUpdate(suffix3, i3, i2, i1, elem), suffix2, suffix1, length)
      else if(i2 < suffix2.length)
        new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, copyUpdate(suffix2, i2, i1, elem), suffix1, length)
      else
        new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, copyUpdate(suffix1, i1, elem), length)
    } else if(index >= len12) {
      val io = index - len12
      new NVector4(prefix1, len1, prefix2, len12, copyUpdate(prefix3, io >> BITS2, (io >> BITS) & MASK, io & MASK, elem), len123, data4, suffix3, suffix2, suffix1, length)
    } else if(index >= len1) {
      val io = index - len1
      new NVector4(prefix1, len1, copyUpdate(prefix2, io >> BITS, io & MASK, elem), len12, prefix3, len123, data4, suffix3, suffix2, suffix1, length)
    } else {
      new NVector4(copyUpdate(prefix1, index, elem), len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, suffix1, length)
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(suffix1.length < WIDTH)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, copyAppend1(suffix1, elem), length+1)
    else if(suffix2.length < WIDTH-1)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, copyAppend(suffix2, suffix1), Array[Any](elem), length+1)
    else if(suffix3.length < WIDTH-1)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, copyAppend(suffix3, copyAppend(suffix2, suffix1)), empty2, Array[Any](elem), length+1)
    else if(data4.length < WIDTH-2)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, copyAppend(data4, copyAppend(suffix3, copyAppend(suffix2, suffix1))), empty3, empty2, Array[Any](elem), length+1)
    else ???
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector4(copyPrepend1(elem, prefix1), len1+1, prefix2, len12+1, prefix3, len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(len12 < WIDTH2)
      new NVector4(Array[Any](elem), 1, copyPrepend(prefix1, prefix2), len12+1, prefix3, len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(len123 < WIDTH3)
      new NVector4(Array[Any](elem), 1, empty2, 1, copyPrepend(copyPrepend(prefix1, prefix2), prefix3), len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(data4.length < WIDTH-2)
      new NVector4(Array[Any](elem), 1, empty2, 1, empty3, 1, copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), data4), suffix3, suffix2, suffix1, length+1)
    else ???
  }

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(prefix2, f)
    foreachElem(prefix3, f)
    foreachElem(data4, f)
    foreachElem(suffix3, f)
    foreachElem(suffix2, f)
    foreachElem(suffix1, f)
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector4(len1=$len1, len12=$len12, len123=$len123, length=$length)\n")
    logArray(sb, prefix1.asInstanceOf[Array[AnyRef]], "  ", "prefix1: ")
    logArray(sb, prefix2, "  ", "prefix2: ")
    logArray(sb, prefix3, "  ", "prefix3: ")
    logArray(sb, data4, "  ", "data4: ")
    logArray(sb, suffix3, "  ", "suffix3: ")
    logArray(sb, suffix2, "  ", "suffix2: ")
    logArray(sb, suffix1.asInstanceOf[Array[AnyRef]], "  ", "suffix1: ")
    sb.result()
  }

  private[collection] def validate(): Unit = {
    assert(data4.length <= (WIDTH-2), s"data4.length is ${data4.length}, should be <= ${WIDTH-2}")
    val p2l = validateArrays(prefix2, "prefix2")
    val p3l = validateArrays(prefix3, "prefix3")
    val d4l = validateArrays(data4, "data4")
    val s3l = validateArrays(suffix3, "suffix3")
    val s2l = validateArrays(suffix2, "suffix2")
    assert(len1 == prefix1.length, s"len1 ($len1) should be prefix1.length (${prefix1.length})")
    assert(len12 == len1 + p2l, s"len12 ($len12) should be len1 + prefix2 length ($p2l)")
    assert(len123 == len1 + p2l + p3l, s"len123 ($len123) should be len1 + prefix2 length ($p2l) + prefix3 length ($p3l)")
    assert(len123 + d4l + s3l + s2l + suffix1.length == length, s"sum of lengths ($len123 + $d4l + $s3l + $s2l + ${suffix1.length}) should be vector length ($length)")
  }

  override def last: A = suffix1(suffix1.length-1).asInstanceOf[A]
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

  override def knownSize: Int = len

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

  def initSparse(size: Int, elem: A): Unit = {
    len = size
    a1 = new Array[Any](WIDTH)
    Arrays.fill(a1.asInstanceOf[Array[AnyRef]], elem.asInstanceOf[AnyRef])
    if(size > WIDTH) {
      a2 = new Array[Array[Any]](WIDTH)
      Arrays.fill(a2.asInstanceOf[Array[AnyRef]], a1)
      if(size > WIDTH2) {
        a3 = new Array[Array[Array[Any]]](WIDTH)
        Arrays.fill(a3.asInstanceOf[Array[AnyRef]], a2)
        if(size > WIDTH3) {
          a4 = new Array[Array[Array[Array[Any]]]](WIDTH)
          Arrays.fill(a4.asInstanceOf[Array[AnyRef]], a3)
          if(size > WIDTH4) {
            a5 = new Array[Array[Array[Array[Array[Any]]]]](WIDTH)
            Arrays.fill(a5.asInstanceOf[Array[AnyRef]], a4)
            if(size > WIDTH5) {
              a6 = new Array[Array[Array[Array[Array[Array[Any]]]]]](LASTWIDTH)
              Arrays.fill(a6.asInstanceOf[Array[AnyRef]], a5)
              depth = 6
            } else depth = 5
          } else depth = 4
        } else depth = 3
      } else depth = 2
    } else depth = 1
  }

  def addOne(elem: A): this.type = {
    val i1 = len & MASK
    if(i1 == 0) advance()
    a1(i1) = elem
    len += 1
    this
  }

  private[this] def shift2(i2: Int): Unit = {
    a1 = new Array[Any](WIDTH)
    a2(i2) = a1
  }

  private[this] def shift3(i3: Int): Unit = {
    a2 = new Array[Array[Any]](WIDTH)
    a3(i3) = a2
  }

  private[this] def shift4(i4: Int): Unit = {
    a3 = new Array[Array[Array[Any]]](WIDTH)
    a4(i4) = a3
  }

  private[this] def shift5(i5: Int): Unit = {
    a4 = new Array[Array[Array[Array[Any]]]](WIDTH)
    a5(i5) = a4
  }

  private[this] def shift6(i6: Int): Unit = {
    a5 = new Array[Array[Array[Array[Array[Any]]]]](WIDTH)
    a6(i6) = a5
  }

  private[this] def advance(): Unit = (depth: @switch) match {
    case 0 =>
      a1 = new Array[Any](WIDTH)
      depth = 1
    case 1 =>
      if(len == WIDTH) {
        a2 = new Array[Array[Any]](WIDTH)
        a2(0) = a1
        shift2(1)
        depth = 2
      }
    case 2 =>
      if(len == WIDTH2) {
        a3 = new Array[Array[Array[Any]]](WIDTH)
        a3(0) = a2
        shift3(1)
        shift2(0)
        depth = 3
      } else {
        val i1 = len & MASK
        val i2 = len >> BITS
        if(i1 == 0) {
          shift2(i2)
        }
      }
    case 3 =>
      if(len == WIDTH3) {
        a4 = new Array[Array[Array[Array[Any]]]](WIDTH)
        a4(0) = a3
        shift4(1)
        shift3(0)
        shift2(0)
        depth = 4
      } else {
        val i1 = len & MASK
        val i2 = (len >> BITS) & MASK
        val i3 = len >> BITS2
        if(i1 == 0) {
          if(i2 == 0) {
            shift3(i3)
          }
          shift2(i2)
        }
      }
    case 4 =>
      if(len == WIDTH4) {
        a5 = new Array[Array[Array[Array[Array[Any]]]]](WIDTH)
        a5(0) = a4
        shift5(1)
        shift4(0)
        shift3(0)
        shift2(0)
        depth = 5
      } else {
        val i1 = len & MASK
        val i2 = (len >> BITS) & MASK
        val i3 = (len >> BITS2) & MASK
        val i4 = len >> BITS3
        if(i1 == 0) {
          if(i2 == 0) {
            if(i3 == 0) {
              shift4(i4)
            }
            shift3(i3)
          }
          shift2(i2)
        }
      }
    case 5 =>
      if(len == WIDTH5) {
        a6 = new Array[Array[Array[Array[Array[Array[Any]]]]]](LASTWIDTH)
        a6(0) = a5
        shift6(1)
        shift5(0)
        shift4(0)
        shift3(0)
        shift2(0)
        depth = 6
      } else {
        val i1 = len & MASK
        val i2 = (len >> BITS) & MASK
        val i3 = (len >> BITS2) & MASK
        val i4 = (len >> BITS3) & MASK
        val i5 = len >> BITS4
        if(i1 == 0) {
          if(i2 == 0) {
            if(i3 == 0) {
              if(i4 == 0) {
                shift5(i5)
              }
              shift4(i4)
            }
            shift3(i3)
          }
          shift2(i2)
        }
      }
    case 6 =>
      if(len == Integer.MAX_VALUE) {
        throw new IndexOutOfBoundsException
      } else {
        val i1 = len & MASK
        val i2 = (len >> BITS) & MASK
        val i3 = (len >> BITS2) & MASK
        val i4 = (len >> BITS3) & MASK
        val i5 = (len >> BITS4) & MASK
        val i6 = len >> BITS5
        if(i1 == 0) {
          if(i2 == 0) {
            if(i3 == 0) {
              if(i4 == 0) {
                if(i5 == 0) {
                  shift6(i6)
                }
                shift5(i5)
              }
              shift4(i4)
            }
            shift3(i3)
          }
          shift2(i2)
        }
      }
  }

  def result(): NVector[A] = /*try*/ {
    if(len == 0) NVector.empty
    else if(len <= WIDTH) {
      if(len == WIDTH) new NVector1(a1)
      else new NVector1(copyOf(a1, len))
    } else if(len <= WIDTH2) {
      val i1 = (len-1) & MASK
      val i2 = (len-1) >> BITS
      val data = Arrays.copyOfRange(a2, 1, i2)
      val prefix1 = a2(0)
      val suffix1 = copyIfDifferentSize(a2(i2), i1+1)
      new NVector2(prefix1, WIDTH, data, suffix1, len)
    } else if(len <= WIDTH3) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >> BITS) & MASK
      val i3 = ((len-1) >> BITS2)
      val data = Arrays.copyOfRange(a3, 1, i3)
      val prefix2 = Arrays.copyOfRange(a3(0), 1, WIDTH)
      val prefix1 = a3(0)(0)
      val suffix2 = Arrays.copyOf(a3(i3), i2)
      val suffix1 = copyIfDifferentSize(a3(i3)(i2), i1+1)
      new NVector3(prefix1, WIDTH, prefix2, WIDTH2, data, suffix2, suffix1, len)
    } else if(len <= WIDTH4) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >> BITS) & MASK
      val i3 = ((len-1) >> BITS2) & MASK
      val i4 = ((len-1) >> BITS3)
      val data = Arrays.copyOfRange(a4, 1, i4)
      val prefix3 = Arrays.copyOfRange(a4(0), 1, WIDTH)
      val prefix2 = Arrays.copyOfRange(a4(0)(0), 1, WIDTH)
      val prefix1 = a4(0)(0)(0)
      val suffix3 = Arrays.copyOf(a4(i4), i3)
      val suffix2 = Arrays.copyOf(a4(i4)(i3), i2)
      val suffix1 = copyIfDifferentSize(a4(i4)(i3)(i2), i1+1)
      new NVector4(prefix1, WIDTH, prefix2, WIDTH2, prefix3, WIDTH3, data, suffix3, suffix2, suffix1, len)
    } else ???
  } //catch { case ex: Exception => println(toDebugString); throw ex }

  private[collection] def toDebugString: String = {
    val i1 = (len-1) & MASK
    val i2 = ((len-1) >> BITS) & MASK
    val i3 = ((len-1) >> BITS2) & MASK
    val i4 = ((len-1) >> BITS3) & MASK
    val i5 = ((len-1) >> BITS4) & MASK
    val i6 = ((len-1) >> BITS5) & MASK
    val sb = new mutable.StringBuilder()
    sb.append(s"NVectorBuilder(len=$len, depth=$depth): i1=$i1, i2=$i2, i3=$i3, i4=$i4, i5=$i5, i6=$i6\n")
    logArray(sb, a6, "  ", "a6: ")
    logArray(sb, a5, "  ", "a5: ", a6, "a6")
    logArray(sb, a4, "  ", "a4: ", a5, "a5")
    logArray(sb, a3, "  ", "a3: ", a4, "a4")
    logArray(sb, a2, "  ", "a2: ", a3, "a3")
    logArray(sb, a1.asInstanceOf[Array[AnyRef]], "  ", "a1: ", a2.asInstanceOf[Array[Array[AnyRef]]], "a2")
    sb.result()
  }
}

private[immutable] object NVectorStatics {
  // compile-time numeric constants
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
  final val LASTWIDTH = WIDTH << 1 // 1 extra bit in the last level to go up to Int.MaxValue (2^31-1) instead of 2^30

  @inline final def copyOf(a: Array[Any], len: Int): Array[Any] =
    Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], len).asInstanceOf[Array[Any]]

  @inline final def copyAppend1(a: Array[Any], elem: Any): Array[Any] = {
    val ac = Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], a.length).asInstanceOf[Array[Any]]
    ac(ac.length-1) = elem
    ac
  }

  @inline final def copyAppend[T <: AnyRef](a: Array[T], elem: T): Array[T] = {
    val ac = Arrays.copyOf(a, a.length)
    ac(ac.length-1) = elem
    ac
  }

  final def copyPrepend1(elem: Any, a: Array[Any]): Array[Any] = {
    val ac = new Array[Any](a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  final def copyPrepend[T <: AnyRef](elem: T, a: Array[T]): Array[T] = {
    val ac = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length+1).asInstanceOf[Array[T]]
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline final def copyIfDifferentSize(a: Array[Any], len: Int): Array[Any] =
    if(a.length == len) a
    else Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], len).asInstanceOf[Array[Any]]

  @inline final def empty1: Array[Any] = new Array(0)
  @inline final def empty2: Array[Array[Any]] = new Array(0)
  @inline final def empty3: Array[Array[Array[Any]]] = new Array(0)
  @inline final def empty4: Array[Array[Array[Array[Any]]]] = new Array(0)

  @inline final def copyUpdate(a1: Array[Any], idx1: Int, elem: Any): Array[Any] = {
    val a1c = a1.clone()
    a1c(idx1) = elem
    a1c
  }

  @inline final def copyUpdate(a2: Array[Array[Any]], idx2: Int, elem: Array[Any]): Array[Array[Any]] = {
    val a2c = a2.clone()
    a2c(idx2) = elem
    a2c
  }

  @inline final def copyUpdate(a2: Array[Array[Any]], idx2: Int, idx1: Int, elem: Any): Array[Array[Any]] = {
    val a2c = a2.clone()
    a2c(idx2) = copyUpdate(a2c(idx2), idx1, elem)
    a2c
  }

  @inline final def copyUpdate(a3: Array[Array[Array[Any]]], idx3: Int, idx2: Int, idx1: Int, elem: Any): Array[Array[Array[Any]]] = {
    val a3c = a3.clone()
    a3c(idx3) = copyUpdate(a3c(idx3), idx2, idx1, elem)
    a3c
  }

  @inline final def copyUpdate(a4: Array[Array[Array[Array[Any]]]], idx4: Int, idx3: Int, idx2: Int, idx1: Int, elem: Any): Array[Array[Array[Array[Any]]]] = {
    val a4c = a4.clone()
    a4c(idx4) = copyUpdate(a4c(idx4), idx3, idx2, idx1, elem)
    a4c
  }

  @inline final def foreachElem[A, U](a1: Array[Any], f: A => U): Unit = {
    var i1 = 0
    while(i1 < a1.length) {
      f(a1(i1).asInstanceOf[A])
      i1 += 1
    }
  }

  final def foreachElem[A, U](a2: Array[Array[Any]], f: A => U): Unit = {
    var i2 = 0
    while(i2 < a2.length) {
      foreachElem(a2(i2), f)
      i2 += 1
    }
  }

  final def foreachElem[A, U](a3: Array[Array[Array[Any]]], f: A => U): Unit = {
    var i3 = 0
    while(i3 < a3.length) {
      foreachElem(a3(i3), f)
      i3 += 1
    }
  }

  final def foreachElem[A, U](a4: Array[Array[Array[Array[Any]]]], f: A => U): Unit = {
    var i4 = 0
    while(i4 < a4.length) {
      foreachElem(a4(i4), f)
      i4 += 1
    }
  }

  final def foreachElem[A, U, T <: AnyRef](n: Int, a: Array[T], f: A => U): Unit = {
    var i = 0
    if(n == 1) foreachElem(a.asInstanceOf[Array[Any]], f)
    else {
      while(i < a.length) {
        foreachElem(n-1, a(i).asInstanceOf[Array[AnyRef]], f)
        i += 1
      }
    }
  }

  final def mapElems1[A, B](a: Array[Any], f: A => B): Array[Any] = {
    val ac: Array[Any] = new Array[Any](a.length)
    var i = 0
    while(i < a.length) {
      val v1 = a(i).asInstanceOf[AnyRef]
      val v2 = f(v1.asInstanceOf[A]).asInstanceOf[AnyRef]
      ac(i) = v2
      i += 1
    }
    ac
  }

  final def mapElems[A, B, T <: AnyRef](n: Int, a: Array[T], f: A => B): Array[T] = {
    if(n == 1)
      mapElems1[A, B](a.asInstanceOf[Array[Any]], f).asInstanceOf[Array[T]]
    else {
      val ac: Array[AnyRef] = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length).asInstanceOf[Array[AnyRef]]
      var i = 0
      while(i < a.length) {
        val v1 = a(i)
        val v2 = mapElems(n-1, v1.asInstanceOf[Array[AnyRef]], f)
        ac(i) = v2
        i += 1
      }
      ac.asInstanceOf[Array[T]]
    }
  }

  /*
  final def mapElems1[A, B](a: Array[Any], f: A => B): Array[Any] = {
    var ac: Array[Any] = null
    var i = 0
    while(i < a.length) {
      val v1 = a(i).asInstanceOf[AnyRef]
      val v2 = f(v1.asInstanceOf[A]).asInstanceOf[AnyRef]
      if(ac ne null) {
        ac(i) = v2
      } else if(v1 ne v2) {
        ac = new Array[Any](a.length)
        var j = 0
        while(j < i) {
          ac(j) = a(j)
          j += 1
        }
        ac(i) = v2
      }
      i += 1
    }
    if(ac ne null) ac else a
  }

  final def mapElems[A, B, T <: AnyRef](n: Int, a: Array[T], f: A => B): Array[T] = {
    if(n == 1)
      mapElems1[A, B](a.asInstanceOf[Array[Any]], f).asInstanceOf[Array[T]]
    else {
      var ac: Array[AnyRef] = null
      var i = 0
      while(i < a.length) {
        val v1 = a(i)
        val v2 = mapElems(n-1, v1.asInstanceOf[Array[AnyRef]], f)
        if(ac ne null) {
          ac(i) = v2
        } else if(v1 ne v2) {
          ac = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length).asInstanceOf[Array[AnyRef]]
          var j = 0
          while(j < i) {
            ac(j) = a(j)
            j += 1
          }
          ac(i) = v2
        }
        i += 1
      }
      if(ac ne null) ac.asInstanceOf[Array[T]] else a
    }
  }
  */

  final def logArray[T <: AnyRef](sb: mutable.StringBuilder, a: Array[T], indent: String = "", prefix: String = "", findIn: Array[Array[T]] = null, findInName: String = "<array>"): Unit = {
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
        val b = new mutable.StringBuilder().append("[")
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
  }

  def validateArrays[T](a: Array[Array[T]], name: String): Int = {
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
}

private[immutable] final class NVector2Iterator[A](a2: Array[Array[Any]], startlength: Int) extends Iterator[A] {
  import NVectorStatics._

  private[this] var pos1, pos2 = 0
  private[this] var rest = startlength
  private[this] var a1: Array[Any] = a2(0)

  override def knownSize = rest

  def hasNext: Boolean = rest > 0

  private[this] def advance(): Unit = {
    if(rest > 0) {
      pos2 += 1
      a1 = a2(pos2)
      pos1 = 0
    }
  }

  def next(): A = {
    if(rest > 0) {
      val r = a1(pos1)
      pos1 += 1
      rest -= 1
      if(pos1 == a1.length) {
        if(rest > 0) {
          pos2 += 1
          a1 = a2(pos2)
          pos1 = 0
        }
      }
      r.asInstanceOf[A]
    } else Iterator.empty.next()
  }

  private[this] def focus(): Unit = {
    val index = startlength - rest
    val len0 = a2(0).length
    if(index < len0) {
      pos2 = 0
      pos1 = index
    } else {
      val io = index + WIDTH - len0
      pos2 = io >> BITS
      pos1 = io & MASK
    }
    a1 = a2(pos2)
  }

  override def drop(n: Int): Iterator[A] = {
    if(n > 0) {
      rest -= Math.min(n, rest)
      focus()
    }
    this
  }
}
