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
import scala.reflect.ClassTag
import java.util.Arrays
import java.util.Arrays.{copyOf, copyOfRange}
import java.lang.Math.{max => mmax, min => mmin, abs}

import NVectorStatics.{Arr1, Arr2, Arr3, Arr4, Arr5, Arr6}

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
          new NVector1[E](a1.asInstanceOf[Array[AnyRef]])
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
  * in O(log n) time, as well as very fast append/prepend/tail/init (amortized O(1), worst case O(log n)).
  * Because vectors strike a good balance between fast random selections and fast random functional updates,
  * they are currently the default implementation of immutable indexed sequences.
  *
  * Vectors are implemented by radix-balanced finger trees of width 32. There is a separate subclass
  * for each level (0 to 6, with 0 being the empty vector and 6 a tree with a maximum width of 64).
  *
  * Tree balancing:
  * - Only the first dimension of an array may have a size < WIDTH
  * - In a `data` (central) array the first dimension may be up to WIDTH-2, in a prefix or suffix WIDTH-1
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
      case ex: Throwable =>
        throw new RuntimeException("Validation failed: " + ex.getMessage + "\n" + toDebugString, ex)
    }
  }

  private[collection] def toDebugString: String

  override def className = "NVector"

  @inline override final def take(n: Int): NVector[A] = slice(0, n)
  @inline override final def drop(n: Int): NVector[A] = slice(n, length)
  @inline override final def takeRight(n: Int): NVector[A] = slice(length - mmax(n, 0), length)
  @inline override final def dropRight(n: Int): NVector[A] = slice(0, length - mmax(n, 0))
  override def tail: NVector[A] = slice(1, length)
  override def init: NVector[A] = slice(0, length-1)

  protected[this] def slice0(lo: Int, hi: Int): NVector[A]

  override final def slice(from: Int, until: Int): NVector[A] = {
    val lo = mmax(from, 0)
    val hi = mmin(until, length)
    val newlen = hi - lo
    if(newlen == length) this
    else if(newlen <= 0) NVector0
    else slice0(lo, hi)
  }

  protected[immutable] def vectorSliceCount: Int
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef]
  protected[immutable] def vectorSliceDim(idx: Int): Int

  override def iterator: Iterator[A] = new NVectorIterator(this)
}

/** Empty vector */
private final object NVector0 extends NVector[Nothing] {
  import NVectorStatics._

  def length = 0
  def apply(index: Int) = throw new IndexOutOfBoundsException

  override def updated[B >: Nothing](index: Int, elem: B): NVector[B] = throw new IndexOutOfBoundsException

  override def appended[B >: Nothing](elem: B): NVector[B] = new NVector1(wrap1(elem))

  override def prepended[B >: Nothing](elem: B): NVector[B] = new NVector1(wrap1(elem))

  override def appendedAll[B >: Nothing](suffix: collection.IterableOnce[B]): NVector[B] =
    NVector.from(suffix)

  override def iterator: Iterator[Nothing] = Iterator.empty

  override def foreach[U](f: Nothing => U): Unit = ()

  private[collection] def validate(): Unit = ()

  private[collection] def toDebugString: String = "NVector0\n"

  override def map[B](f: Nothing => B): NVector[B] = this

  override def tail: NVector[Nothing] = throw new UnsupportedOperationException("empty.tail")

  override def init: NVector[Nothing] = throw new UnsupportedOperationException("empty.init")

  protected[this] def slice0(lo: Int, hi: Int): NVector[Nothing] = this

  protected[immutable] def vectorSliceCount: Int = 0
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = null
  protected[immutable] def vectorSliceDim(idx: Int): Int = -1
}

/** Flat ArraySeq-like structure.
  *
  * @param data1 The vector's content, with length between 1 and WIDTH.
  */
private final class NVector1[+A](data1: Array[AnyRef]) extends NVector[A] {
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
      a(length) = elem.asInstanceOf[AnyRef]
      new NVector1(a)
    } else new NVector2(data1, WIDTH, empty2, wrap1(elem), WIDTH+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) new NVector1(copyPrepend1(elem, data1))
    else new NVector2(wrap1(elem), 1, empty2, data1, length+1)
  }

  override def iterator: Iterator[A] = new ArrayOps.ArrayIterator(data1).asInstanceOf[Iterator[A]]

  override def foreach[U](f: A => U): Unit = foreachElem(data1, f)

  private[collection] def validate(): Unit = {
    assert(length > 0 && length <= WIDTH, s"length is $length, should be > 0 and <= ${WIDTH}")
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector1\n")
    logArray(sb, data1, "  ", "data1: ")
    sb.result()
  }

  override def map[B](f: A => B): NVector[B] = new NVector1(mapElems1(data1, f))

  override def head: A = data1(0).asInstanceOf[A]

  override def last: A = data1(data1.length-1).asInstanceOf[A]

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] =
    new NVector1(copyOfRange(data1, lo, hi))

  override def tail: NVector[A] =
    if(data1.length == 1) NVector0
    else new NVector1(copyTail(data1))

  override def init: NVector[A] =
    if(data1.length == 1) NVector0
    else new NVector1(copyInit(data1))

  protected[immutable] def vectorSliceCount: Int = 1
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = data1
  protected[immutable] def vectorSliceDim(idx: Int): Int = 1
}

/** 2-level radix tree with fingers at both ends.
  *
  * @param prefix1 The level 1 prefix
  * @param len1 The length of prefix1
  * @param data2 The main data, excluding prefix and suffix.
  * @param suffix1 The level 1 suffix
  * @param length The actual number of elements in the vector
  */
private final class NVector2[+A](prefix1: Array[AnyRef], len1: Int,
                                 data2: Arr2,
                                 suffix1: Array[AnyRef],
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
      new NVector2(prefix1, len1, data2, copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length+1)
    else if(data2.length < WIDTH-2)
      new NVector2(prefix1, len1, copyAppend(data2, suffix1), wrap1(elem), length+1)
    else
      new NVector3(prefix1, len1, data2, WIDTH*(WIDTH-2) + len1, empty3, wrap2(suffix1), wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector2(copyPrepend1(elem, prefix1), len1+1, data2, suffix1, length+1)
    else if(data2.length < WIDTH-2)
      new NVector2(wrap1(elem), 1, copyPrepend(prefix1, data2), suffix1, length+1)
    else
      new NVector3(wrap1(elem), 1, wrap2(prefix1), len1+1, empty3, data2, suffix1, length+1)
  }

  //override def iterator: Iterator[A] = new NVectorIterator(length, Array[AnyRef](prefix1, data2, suffix1))

  override def foreach[U](f: A => U): Unit = {
    foreachElem(prefix1, f)
    foreachElem(data2, f)
    foreachElem(suffix1, f)
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector2(len1=$len1, length=$length)\n")
    logArray(sb, prefix1, "  ", "prefix1: ")
    logArray(sb, data2, "  ", "data2: ")
    logArray(sb, suffix1, "  ", "suffix1: ")
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

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, data2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) new NVector2(copyTail(prefix1), len1-1, data2, suffix1, length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) new NVector2(prefix1, len1, data2, copyInit(suffix1), length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 3
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => data2
    case 2 => suffix1
  }
  protected[immutable] def vectorSliceDim(idx: Int): Int = 2-abs(idx-1)
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
private final class NVector3[+A](prefix1: Array[AnyRef], len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 data3: Arr3,
                                 suffix2: Arr2, suffix1: Array[AnyRef],
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
      new NVector3(prefix1, len1, prefix2, len12, data3, suffix2, copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length+1)
    else if(suffix2.length < WIDTH-1)
      new NVector3(prefix1, len1, prefix2, len12, data3, copyAppend(suffix2, suffix1), wrap1(elem), length+1)
    else if(data3.length < WIDTH-2)
      new NVector3(prefix1, len1, prefix2, len12, copyAppend(data3, copyAppend(suffix2, suffix1)), empty2, wrap1(elem), length+1)
    else
      new NVector4(prefix1, len1, prefix2, len12, data3, (WIDTH-2)*WIDTH2 + len12, empty4, wrap3(copyAppend(suffix2, suffix1)), empty2, wrap1(elem), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector3(copyPrepend1(elem, prefix1), len1+1, prefix2, len12+1, data3, suffix2, suffix1, length+1)
    else if(len12 < WIDTH2)
      new NVector3(wrap1(elem), 1, copyPrepend(prefix1, prefix2), len12+1, data3, suffix2, suffix1, length+1)
    else if(data3.length < WIDTH-2)
      new NVector3(wrap1(elem), 1, empty2, len1, copyPrepend(copyPrepend(prefix1, prefix2), data3), suffix2, suffix1, length+1)
    else
      new NVector4(wrap1(elem), 1, empty2, 1, wrap3(copyPrepend(prefix1, prefix2)), len12+1, empty4, data3, suffix2, suffix1, length+1)
  }

  //override def iterator: Iterator[A] = new NVectorIterator(length, Array[AnyRef](prefix1, prefix2, data3, suffix2, suffix1))

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
    logArray(sb, prefix1, "  ", "prefix1: ")
    logArray(sb, prefix2, "  ", "prefix2: ")
    logArray(sb, data3, "  ", "data3: ")
    logArray(sb, suffix2, "  ", "suffix2: ")
    logArray(sb, suffix1, "  ", "suffix1: ")
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

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, data3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) new NVector3(copyTail(prefix1), len1-1, prefix2, len12-1, data3, suffix2, suffix1, length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) new NVector3(prefix1, len1, prefix2, len12, data3, suffix2, copyInit(suffix1), length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 5
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => data3
    case 3 => suffix2
    case 4 => suffix1
  }
  protected[immutable] def vectorSliceDim(idx: Int): Int = 3-abs(idx-2)
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
private final class NVector4[+A](prefix1: Array[AnyRef], len1: Int,
                                 prefix2: Arr2, len12: Int,
                                 prefix3: Arr3, len123: Int,
                                 data4: Arr4,
                                 suffix3: Arr3, suffix2: Arr2, suffix1: Array[AnyRef],
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
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, copyAppend(suffix1, elem.asInstanceOf[AnyRef]), length+1)
    else if(suffix2.length < WIDTH-1)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, copyAppend(suffix2, suffix1), wrap1(elem), length+1)
    else if(suffix3.length < WIDTH-1)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, copyAppend(suffix3, copyAppend(suffix2, suffix1)), empty2, wrap1(elem), length+1)
    else if(data4.length < WIDTH-2)
      new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, copyAppend(data4, copyAppend(suffix3, copyAppend(suffix2, suffix1))), empty3, empty2, wrap1(elem), length+1)
    else ???
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector4(copyPrepend1(elem, prefix1), len1+1, prefix2, len12+1, prefix3, len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(len12 < WIDTH2)
      new NVector4(wrap1(elem), 1, copyPrepend(prefix1, prefix2), len12+1, prefix3, len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(len123 < WIDTH3)
      new NVector4(wrap1(elem), 1, empty2, 1, copyPrepend(copyPrepend(prefix1, prefix2), prefix3), len123+1, data4, suffix3, suffix2, suffix1, length+1)
    else if(data4.length < WIDTH-2)
      new NVector4(wrap1(elem), 1, empty2, 1, empty3, 1, copyPrepend(copyPrepend(copyPrepend(prefix1, prefix2), prefix3), data4), suffix3, suffix2, suffix1, length+1)
    else ???
  }

  //override def iterator: Iterator[A] = new NVectorIterator(length, Array[AnyRef](prefix1, prefix2, prefix3, data4, suffix3, suffix2, suffix1))

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
    logArray(sb, prefix1, "  ", "prefix1: ")
    logArray(sb, prefix2, "  ", "prefix2: ")
    logArray(sb, prefix3, "  ", "prefix3: ")
    logArray(sb, data4, "  ", "data4: ")
    logArray(sb, suffix3, "  ", "suffix3: ")
    logArray(sb, suffix2, "  ", "suffix2: ")
    logArray(sb, suffix1, "  ", "suffix1: ")
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

  protected[this] def slice0(lo: Int, hi: Int): NVector[A] = {
    val b = new NVectorSliceBuilder(lo, hi)
    b.consider(1, prefix1)
    b.consider(2, prefix2)
    b.consider(3, prefix3)
    b.consider(4, data4)
    b.consider(3, suffix3)
    b.consider(2, suffix2)
    b.consider(1, suffix1)
    b.result()
  }

  override def tail: NVector[A] =
    if(len1 > 1) new NVector4(copyTail(prefix1), len1-1, prefix2, len12-1, prefix3, len123-1, data4, suffix3, suffix2, suffix1, length-1)
    else slice0(1, length)

  override def init: NVector[A] =
    if(suffix1.length > 1) new NVector4(prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, copyInit(suffix1), length-1)
    else slice0(0, length-1)

  protected[immutable] def vectorSliceCount: Int = 7
  protected[immutable] def vectorSlice(idx: Int): Array[_ <: AnyRef] = (idx: @switch) match {
    case 0 => prefix1
    case 1 => prefix2
    case 2 => prefix3
    case 3 => data4
    case 4 => suffix3
    case 5 => suffix2
    case 6 => suffix1
  }
  protected[immutable] def vectorSliceDim(idx: Int): Int = 4-abs(idx-3)
}

private[immutable] final class NVectorSliceBuilder(lo: Int, hi: Int) {
  //println(s"***** NVectorSliceBuilder($lo, $hi)")
  import NVectorStatics._

  private[this] val slices = new Array[Array[AnyRef]](11)
  private[this] var len, pos, maxDim = 0

  @inline private[this] def prefixIdx(n: Int) = n-1
  private[this] final val DATA6 = 5
  @inline private[this] def suffixIdx(n: Int) = 11-n

  def consider[T <: AnyRef](n: Int, a: Array[T]): Unit = {
    //println(s"*****   consider($n, /${a.length})")
    val count = a.length * (1 << (BITS*(n-1)))
    val lo0 = mmax(lo-pos, 0)
    val hi0 = mmin(hi-pos, count)
    if(hi0 > lo0) addSlice(n, a, lo0, hi0)
    pos += count
  }

  private[this] def addSlice[T <: AnyRef](n: Int, a: Array[T], lo: Int, hi: Int): Unit = {
    //println(s"*****     addSlice($n, /${a.length}, $lo, $hi)")
    if(n == 1) {
      add(1, copyOrUse(a, lo, hi))
    } else {
      val bitsN = BITS * (n-1)
      var loN = lo >> bitsN
      var hiN = hi >> bitsN
      if(n != 6) {
        loN &= MASK
        hiN &= MASK
      }
      val loRest = lo & ((1 << bitsN) - 1)
      val hiRest = hi & ((1 << bitsN) - 1)
      //println(s"*****       bitsN=$bitsN, loN=$loN, hiN=$hiN, loRest=$loRest, hiRest=$hiRest")
      if(loRest == 0) {
        if(hiRest == 0) {
          add(n, copyOrUse(a, loN, hiN))
        } else {
          if(hiN > loN) add(n, copyOrUse(a, loN, hiN))
          addSlice(n-1, a(hiN).asInstanceOf[Array[AnyRef]], 0, hiRest)
        }
      } else {
        addSlice(n-1, a(loN).asInstanceOf[Array[AnyRef]], loRest, 1 << bitsN)
        if(hiRest == 0) {
          if(hiN > loN+1) add(n, copyOrUse(a, loN+1, hiN))
        } else {
          if(hiN-1 > loN+1) add(n, copyOrUse(a, loN+1, hiN-1))
          addSlice(n-1, a(hiN-1).asInstanceOf[Array[AnyRef]], 0, hiRest)
        }
      }
    }
  }

  private[this] def add[T <: AnyRef](n: Int, a: Array[T]): Unit = {
    val idx =
      if(n <= maxDim) suffixIdx(n)
      else { maxDim = n; prefixIdx(n) }
    slices(idx) = a.asInstanceOf[Array[AnyRef]]
    len += (a.length * (1 << (BITS*(n-1))))
  }

  def result[A](): NVector[A] = {
    if(len <= 32) {
      if(len == 0) NVector0
      else {
        val prefix1 = slices(prefixIdx(1))
        val suffix1 = slices(suffixIdx(1))
        val a: Arr1 =
          if(prefix1 ne null) {
            if(suffix1 ne null) concatArrays(prefix1, suffix1)
            else prefix1
          } else if(suffix1 ne null) suffix1
          else {
            val prefix2 = slices(prefixIdx(2)).asInstanceOf[Arr2]
            if(prefix2 ne null) prefix2(0)
            else {
              val suffix2 = slices(suffixIdx(2)).asInstanceOf[Arr2]
              suffix2(0)
            }
          }
        new NVector1(a)
      }
    } else {
      balancePrefix(1)
      balanceSuffix(1)
      var resultDim = maxDim
      if(resultDim < 6) {
        val pre = slices(prefixIdx(maxDim))
        val suf = slices(suffixIdx(maxDim))
        if((pre ne null) && (suf ne null)) {
          if(pre.length + suf.length <= WIDTH-2) {
            slices(prefixIdx(maxDim)) = concatArrays(pre, suf)
            slices(suffixIdx(maxDim)) = null
          } else resultDim += 1
        } else {
          val one = if(pre ne null) pre else suf
          if(one.length > WIDTH-2) resultDim += 1
        }
      }
      val prefix1 = slices(prefixIdx(1))
      val suffix1 = slices(suffixIdx(1))
      val len1 = prefix1.length
      val res = (resultDim: @switch) match {
        case 2 =>
          val data2 = dataOr(2, empty2)
          new NVector2[A](prefix1, len1, data2, suffix1, len)
        case 3 =>
          val prefix2 = prefixOr(2, empty2)
          val data3 = dataOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          new NVector3[A](prefix1, len1, prefix2, len12, data3, suffix2, suffix1, len)
        case 4 =>
          val prefix2 = prefixOr(2, empty2)
          val prefix3 = prefixOr(3, empty3)
          val data4 = dataOr(4, empty4)
          val suffix3 = suffixOr(3, empty3)
          val suffix2 = suffixOr(2, empty2)
          val len12 = len1 + (prefix2.length * WIDTH)
          val len123 = len12 + (prefix3.length * WIDTH2)
          new NVector4[A](prefix1, len1, prefix2, len12, prefix3, len123, data4, suffix3, suffix2, suffix1, len)
        case _ =>
          ???
      }
      //res.validateDebug()
      res
    }
  }

  @inline private[this] def prefixOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val p = slices(prefixIdx(n))
    if(p ne null) p.asInstanceOf[Array[T]] else a
  }

  @inline private[this] def suffixOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val s = slices(suffixIdx(n))
    if(s ne null) s.asInstanceOf[Array[T]] else a
  }

  @inline private[this] def dataOr[T <: AnyRef](n: Int, a: Array[T]): Array[T] = {
    val p = slices(prefixIdx(n))
    if(p ne null) p.asInstanceOf[Array[T]]
    else {
      val s = slices(suffixIdx(n))
      if(s ne null) s.asInstanceOf[Array[T]] else a
    }
  }

  private[this] def balancePrefix(n: Int): Unit = {
    if(slices(prefixIdx(n)) eq null) {
      if(n == maxDim) {
        slices(prefixIdx(n)) = slices(suffixIdx(n))
        slices(suffixIdx(n)) = null
      } else {
        balancePrefix(n+1)
        val preN1 = slices(prefixIdx(n+1)).asInstanceOf[Array[Array[AnyRef]]]
        //assert(preN1 ne null)
        slices(prefixIdx(n)) = preN1(0)
        if(preN1.length == 1) {
          slices(prefixIdx(n+1)) = null
          if((maxDim == n+1) && (slices(suffixIdx(n+1)) eq null)) maxDim = n
        } else {
          slices(prefixIdx(n+1)) = copyOfRange(preN1, 1, preN1.length).asInstanceOf[Array[AnyRef]]
        }
      }
    }
  }

  private[this] def balanceSuffix(n: Int): Unit = {
    if(slices(suffixIdx(n)) eq null) {
      if(n == maxDim) {
        slices(suffixIdx(n)) = slices(prefixIdx(n))
        slices(prefixIdx(n)) = null
      } else {
        balanceSuffix(n+1)
        val sufN1 = slices(suffixIdx(n+1)).asInstanceOf[Array[Array[AnyRef]]]
        //assert(sufN1 ne null, s"n=$n, maxDim=$maxDim, slices=${slices.mkString(",")}")
        slices(suffixIdx(n)) = sufN1(sufN1.length-1)
        if(sufN1.length == 1) {
          slices(suffixIdx(n+1)) = null
          if((maxDim == n+1) && (slices(prefixIdx(n+1)) eq null)) maxDim = n
        } else {
          slices(suffixIdx(n+1)) = copyOfRange(sufN1, 0, sufN1.length-1).asInstanceOf[Array[AnyRef]]
        }
      }
    }
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVectorSliceBuilder(lo=$lo, hi=$hi, len=$len, pos=$pos, maxDim=$maxDim)\n")
    logArray(sb, slices, "  ", "slices: ")
    sb.result()
  }

  @inline private[this] def copyOrUse[T <: AnyRef](a: Array[T], start: Int, end: Int): Array[T] =
    if(start == 0 && end == a.length) a else copyOfRange[T](a, start, end)
}

private final class NVectorBuilder[A] extends ReusableBuilder[A, NVector[A]] {
  import NVectorStatics._

  private[this] var a6: Arr6 = _
  private[this] var a5: Arr5 = _
  private[this] var a4: Arr4 = _
  private[this] var a3: Arr3 = _
  private[this] var a2: Arr2 = _
  private[this] var a1: Arr1 = _
  private[this] var len, depth: Int = 0

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
    a1 = new Array(WIDTH)
    Arrays.fill(a1, elem)
    if(size > WIDTH) {
      a2 = new Array(WIDTH)
      Arrays.fill(a2.asInstanceOf[Array[AnyRef]], a1)
      if(size > WIDTH2) {
        a3 = new Array(WIDTH)
        Arrays.fill(a3.asInstanceOf[Array[AnyRef]], a2)
        if(size > WIDTH3) {
          a4 = new Array(WIDTH)
          Arrays.fill(a4.asInstanceOf[Array[AnyRef]], a3)
          if(size > WIDTH4) {
            a5 = new Array(WIDTH)
            Arrays.fill(a5.asInstanceOf[Array[AnyRef]], a4)
            if(size > WIDTH5) {
              a6 = new Array(LASTWIDTH)
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
    a1(i1) = elem.asInstanceOf[AnyRef]
    len += 1
    this
  }

  private[this] def shift2(i2: Int): Unit = { a1 = new Array(WIDTH); a2(i2) = a1 }
  private[this] def shift3(i3: Int): Unit = { a2 = new Array(WIDTH); a3(i3) = a2 }
  private[this] def shift4(i4: Int): Unit = { a3 = new Array(WIDTH); a4(i4) = a3 }
  private[this] def shift5(i5: Int): Unit = { a4 = new Array(WIDTH); a5(i5) = a4 }
  private[this] def shift6(i6: Int): Unit = { a5 = new Array(WIDTH); a6(i6) = a5 }

  private[this] def advance(): Unit = (depth: @switch) match {
    case 0 =>
      a1 = new Array(WIDTH)
      depth = 1
    case 1 =>
      if(len == WIDTH) {
        a2 = new Array(WIDTH)
        a2(0) = a1
        shift2(1)
        depth = 2
      }
    case 2 =>
      if(len == WIDTH2) {
        a3 = new Array(WIDTH)
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
        a4 = new Array(WIDTH)
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
        a5 = new Array(WIDTH)
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
        a6 = new Array(LASTWIDTH)
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
      val data = copyOfRange(a2, 1, i2)
      val prefix1 = a2(0)
      val suffix1 = copyIfDifferentSize(a2(i2), i1+1)
      new NVector2(prefix1, WIDTH, data, suffix1, len)
    } else if(len <= WIDTH3) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >> BITS) & MASK
      val i3 = ((len-1) >> BITS2)
      val data = copyOfRange(a3, 1, i3)
      val prefix2 = copyOfRange(a3(0), 1, WIDTH)
      val prefix1 = a3(0)(0)
      val suffix2 = copyOf(a3(i3), i2)
      val suffix1 = copyIfDifferentSize(a3(i3)(i2), i1+1)
      new NVector3(prefix1, WIDTH, prefix2, WIDTH2, data, suffix2, suffix1, len)
    } else if(len <= WIDTH4) {
      val i1 = (len-1) & MASK
      val i2 = ((len-1) >> BITS) & MASK
      val i3 = ((len-1) >> BITS2) & MASK
      val i4 = ((len-1) >> BITS3)
      val data = copyOfRange(a4, 1, i4)
      val prefix3 = copyOfRange(a4(0), 1, WIDTH)
      val prefix2 = copyOfRange(a4(0)(0), 1, WIDTH)
      val prefix1 = a4(0)(0)(0)
      val suffix3 = copyOf(a4(i4), i3)
      val suffix2 = copyOf(a4(i4)(i3), i2)
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
    logArray(sb, a1, "  ", "a1: ", a2, "a2")
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

  type Arr1 = Array[AnyRef]
  type Arr2 = Array[Array[AnyRef]]
  type Arr3 = Array[Array[Array[AnyRef]]]
  type Arr4 = Array[Array[Array[Array[AnyRef]]]]
  type Arr5 = Array[Array[Array[Array[Array[AnyRef]]]]]
  type Arr6 = Array[Array[Array[Array[Array[Array[AnyRef]]]]]]

  final def copyAppend[T <: AnyRef](a: Array[T], elem: T): Array[T] = {
    val ac = copyOf(a, a.length)
    ac(ac.length-1) = elem
    ac
  }

  final def copyPrepend1(elem: Any, a: Arr1): Arr1 = {
    val ac = new Arr1(a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem.asInstanceOf[AnyRef]
    ac
  }

  final def copyPrepend[T <: AnyRef](elem: T, a: Array[T]): Array[T] = {
    val ac = java.lang.reflect.Array.newInstance(a.getClass.getComponentType, a.length+1).asInstanceOf[Array[T]]
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline final def copyTail[T <: AnyRef](a: Array[T]): Array[T] = copyOfRange[T](a, 1, a.length)

  @inline final def copyInit[T <: AnyRef](a: Array[T]): Array[T] = copyOfRange[T](a, 0, a.length-1)

  @inline final def copyIfDifferentSize[T <: AnyRef](a: Array[T], len: Int): Array[T] =
    if(a.length == len) a else copyOf[T](a, len)

  final val empty1: Arr1 = new Array(0)
  final val empty2: Arr2 = new Array(0)
  final val empty3: Arr3 = new Array(0)
  final val empty4: Arr4 = new Array(0)

  @inline final def wrap1(x: Any ): Arr1 = { val a = new Arr1(1); a(0) = x.asInstanceOf[AnyRef]; a }
  @inline final def wrap2(x: Arr1): Arr2 = { val a = new Arr2(1); a(0) = x; a }
  @inline final def wrap3(x: Arr2): Arr3 = { val a = new Arr3(1); a(0) = x; a }
  @inline final def wrap4(x: Arr3): Arr4 = { val a = new Arr4(1); a(0) = x; a }

  @inline final def copyUpdate(a1: Arr1, idx1: Int, elem: Any): Arr1 = {
    val a1c = a1.clone()
    a1c(idx1) = elem.asInstanceOf[AnyRef]
    a1c
  }

  @inline final def copyUpdate(a2: Arr2, idx2: Int, elem: Arr1): Arr2 = {
    val a2c = a2.clone()
    a2c(idx2) = elem
    a2c
  }

  @inline final def copyUpdate(a2: Arr2, idx2: Int, idx1: Int, elem: Any): Arr2 = {
    val a2c = a2.clone()
    a2c(idx2) = copyUpdate(a2c(idx2), idx1, elem)
    a2c
  }

  @inline final def copyUpdate(a3: Arr3, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr3 = {
    val a3c = a3.clone()
    a3c(idx3) = copyUpdate(a3c(idx3), idx2, idx1, elem)
    a3c
  }

  @inline final def copyUpdate(a4: Arr4, idx4: Int, idx3: Int, idx2: Int, idx1: Int, elem: Any): Arr4 = {
    val a4c = a4.clone()
    a4c(idx4) = copyUpdate(a4c(idx4), idx3, idx2, idx1, elem)
    a4c
  }

  @inline final def concatArrays[T <: AnyRef](a: Array[T], b: Array[T]): Array[T] = {
    val dest = copyOf[T](a, a.length+b.length)
    System.arraycopy(b, 0, dest, a.length, b.length)
    dest
  }

  @inline final def foreachElem[A, U](a1: Arr1, f: A => U): Unit = {
    var i1 = 0
    while(i1 < a1.length) {
      f(a1(i1).asInstanceOf[A])
      i1 += 1
    }
  }

  final def foreachElem[A, U](a2: Arr2, f: A => U): Unit = {
    var i2 = 0
    while(i2 < a2.length) {
      foreachElem(a2(i2), f)
      i2 += 1
    }
  }

  final def foreachElem[A, U](a3: Arr3, f: A => U): Unit = {
    var i3 = 0
    while(i3 < a3.length) {
      foreachElem(a3(i3), f)
      i3 += 1
    }
  }

  final def foreachElem[A, U](a4: Arr4, f: A => U): Unit = {
    var i4 = 0
    while(i4 < a4.length) {
      foreachElem(a4(i4), f)
      i4 += 1
    }
  }

  final def foreachElem[A, U, T <: AnyRef](n: Int, a: Array[T], f: A => U): Unit = {
    var i = 0
    if(n == 1) foreachElem(a.asInstanceOf[Array[AnyRef]], f)
    else {
      while(i < a.length) {
        foreachElem(n-1, a(i).asInstanceOf[Array[AnyRef]], f)
        i += 1
      }
    }
  }

  final def mapElems1[A, B](a: Arr1, f: A => B): Arr1 = {
    val ac: Arr1 = new Array(a.length)
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
      mapElems1[A, B](a.asInstanceOf[Arr1], f).asInstanceOf[Array[T]]
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
  final def mapElems1[A, B](a: Arr1, f: A => B): Arr1 = {
    var ac: Arr1 = null
    var i = 0
    while(i < a.length) {
      val v1 = a(i).asInstanceOf[AnyRef]
      val v2 = f(v1.asInstanceOf[A]).asInstanceOf[AnyRef]
      if(ac ne null) {
        ac(i) = v2
      } else if(v1 ne v2) {
        ac = new Array(a.length)
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
      mapElems1[A, B](a.asInstanceOf[Arr1], f).asInstanceOf[Array[T]]
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

  final def logArray[T <: AnyRef](sb: mutable.StringBuilder, a: Array[T], indent: String = "", prefix: String = "", findIn: Array[Array[T]] = null, findInName: String = "<array>"): mutable.StringBuilder = {
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
    sb
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

private[immutable] final class NVectorIterator[A](v: NVector[A]) extends Iterator[A] {
  import NVectorStatics._

  private[this] val totalLength = v.length
  private[this] var len1 = totalLength // length relative to i1
  private[this] var a1: Arr1 = _
  private[this] var slice: Array[_ <: AnyRef] = _
  private[this] var sliceIdx, sliceDim = -1
  private[this] var sliceStart, sliceEnd, i1 = 0

  advanceA1()

  @inline override def knownSize = len1 - i1

  @inline def hasNext: Boolean = len1 > i1

  private[this] def advanceSlice(pos: Int): Unit = {
    if(!hasNext) Iterator.empty.next()
    slice = null
    while((slice eq null) || slice.length == 0) {
      sliceIdx += 1
      slice = v.vectorSlice(sliceIdx)
    }
    sliceStart = pos
    sliceDim = v.vectorSliceDim(sliceIdx)
    sliceEnd = sliceStart + slice.length * (1 << (BITS*(sliceDim-1)))
  }

  private[this] def advanceA1(): Unit = {
    val pos = i1-len1+totalLength
    if(pos == sliceEnd) advanceSlice(pos)
    val io = pos - sliceStart
    a1 = (sliceDim: @switch) match {
      case 1 => slice.asInstanceOf[Arr1]
      case 2 => slice.asInstanceOf[Arr2](io >> BITS)
      case 3 => slice.asInstanceOf[Arr3](io >> BITS2)((io >> BITS) & MASK)
      case 4 => slice.asInstanceOf[Arr4](io >> BITS3)((io >> BITS2) & MASK)((io >> BITS) & MASK)
      case 5 => slice.asInstanceOf[Arr5](io >> BITS4)((io >> BITS3) & MASK)((io >> BITS2) & MASK)((io >> BITS) & MASK)
      case 6 => slice.asInstanceOf[Arr6](io >> BITS5)((io >> BITS4) & MASK)((io >> BITS3) & MASK)((io >> BITS2) & MASK)((io >> BITS) & MASK)
    }
    len1 -= i1
    i1 = 0
  }

  def next(): A = {
    if(i1 == a1.length) advanceA1()
    val r = a1(i1)
    i1 += 1
    r.asInstanceOf[A]
  }
}
