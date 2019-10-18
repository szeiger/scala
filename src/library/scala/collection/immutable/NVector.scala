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

  private[collection] def validateDebug(): Unit = {
    try validate() catch {
      case ex: Exception =>
        throw new RuntimeException("Validation failed: " + ex.getMessage + "\n" + toDebugString, ex)
    }
  }

  private[collection] def toDebugString: String
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
      //new NVector2(WIDTH, a2, len)
      if(len == WIDTH2)
        new NVector2(WIDTH, a2, len)
      else {
        val i1 = (len-1) & MASK
        val i2 = (len-1) >> BITS
        val data = Arrays.copyOf(a2, i2+1)
        if(i1+1 != WIDTH)
          data(data.length-1) = copyOf(data(data.length-1), i1+1)
        new NVector2(WIDTH, data, len)
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
      new NVector3(prefix1, WIDTH, prefix2, WIDTH2, data, suffix2, suffix1, len)
    } else ???
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVectorBuilder(len=$len, depth=$depth)\n")
    logArray(sb, a6, "  ", "a6: ")
    logArray(sb, a5, "  ", "a5: ")
    logArray(sb, a4, "  ", "a4: ")
    logArray(sb, a3, "  ", "a3: ")
    logArray(sb, a2, "  ", "a2: ")
    logArray(sb, a1.asInstanceOf[Array[AnyRef]], "  ", "a1: ")
    sb.result()
  }
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

  private[collection] def validate(): Unit = ()

  private[collection] def toDebugString: String = "NVector0\n"
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
    new NVector1(copyUpdate(data, index, elem))
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) {
      val a = copyOf(data, length+1)
      a(length) = elem
      new NVector1(a)
    } else new NVector2(WIDTH, Array[Array[Any]](data, Array[Any](elem)), length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(length < WIDTH) new NVector1(copyPrepend(elem, data))
    else new NVector2(1, Array[Array[Any]](Array[Any](elem), data), length+1)
  }

  private[collection] def validate(): Unit = {
    assert(length > 0 && length <= WIDTH, s"length is $length, should be > 0 and <= ${WIDTH}")
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector1\n")
    logArray(sb, data.asInstanceOf[Array[AnyRef]], "  ", "data: ")
    sb.result()
  }
}

/** Simple 2-level radix tree with variable-size first element (for amortized O(1) prepend).
  *
  * @param len0 The length of data(0)
  * @param data The radix tree with the content in the 2nd level. Length of the top-level array and the first
  *             and last sub-array is between 1 and WIDTH, all other sub-arrays have length WIDTH.
  * @param length The actual number of elements in the vector
  */
private final class NVector2[+A](len0: Int, data: Array[Array[Any]], val length: Int) extends NVector[A] {
  import NVectorStatics._

  @inline def apply(index: Int): A = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index < len0) data(0)(index)
    else {
      val io = index + WIDTH - len0
      data(io >> BITS)(io & MASK)
    }
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    val a2 =
      if(index >= len0) {
        val io = index + WIDTH - len0
        copyUpdate(data, io >> BITS, io & MASK, elem)
      } else copyUpdate(data, 0, index, elem)
    new NVector2(len0, a2, length)
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(length-len0 < WIDTH2-WIDTH) {
      var a1 = data(data.length-1)
      if(a1.length < WIDTH)
        new NVector2(len0, copyUpdate(data, data.length-1, copyAppend(a1, elem)), length+1)
      else
        new NVector2(len0, copyAppend(data, Array[Any](elem)), length+1)
    } else new NVector3(data(0), len0,
                        Arrays.copyOfRange(data, 1, data.length), length,
                        empty3,
                        empty2, Array[Any](elem),
                        length+1)
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len0 < WIDTH) {
      val a2 = Arrays.copyOf[Array[Any]](data, data.length)
      a2(0) = copyPrepend(elem, a2(0))
      new NVector2(len0+1, a2, length+1)
    } else if(length < WIDTH2)
      new NVector2(1, copyPrepend(Array[Any](elem), data), length+1)
    else new NVector3(Array[Any](elem), 1,
                      empty2, 1,
                      empty3,
                      Arrays.copyOf(data, data.length-1), data(data.length-1),
                      length+1)
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector2(len0=$len0, length=$length)\n")
    logArray(sb, data, "  ", "data: ")
    sb.result()
  }

  private[collection] def validate(): Unit = {
    assert((length-len0) > 0 && (length-len0) <= (WIDTH2-WIDTH), s"length ($length) - len0 ($len0) should be > 0 and <= ${WIDTH2-WIDTH}")
    assert(data.length > 0 && data.length <= WIDTH, s"data.length is ${data.length}, should be > 0 and <= $WIDTH")
    assert(data.forall(_ ne null), "data should not contain null entries")
    assert(data.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all arrays should be > 0 and <= $WIDTH")
    val sum = data.map(_.length).sum
    assert(sum == length, s"sum of data lengths ($sum) should be vector length ($length)")
    assert(data(0).length == len0, s"data(0).length (${data(0).length}) should be len0 ($len0)")
  }
}

/** 3-level radix tree with fingers at both ends. Max size is WIDTH for prefix1 and suffix2, WIDTH-1 for
  * prefix2 and suffix1, and WIDTH-2 for data.
  *
  * @param prefix1 The level 1 prefix
  * @param len1 The length of prefix1
  * @param prefix2 The level 2 prefix
  * @param len12 The combined length of prefix 1 and all prefix2 subarrays
  * @param data The main data, excluding prefix and suffix.
  * @param suffix 2 The level 2 suffix
  * @param suffix 1 The level 1 suffix
  * @param length The actual number of elements in the vector
  */
private final class NVector3[+A](prefix1: Array[Any], len1: Int,
                                 prefix2: Array[Array[Any]], len12: Int,
                                 data: Array[Array[Array[Any]]],
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
      if(i3 < data.length) data(i3)(i2)(i1)
      else if(i2 < suffix2.length) suffix2(i2)(i1)
      else suffix1(i1)
    } else if(index >= len1) {
      val ip = index - len1
      prefix2(ip >> BITS)(ip & MASK)
    } else prefix1(index)
  }.asInstanceOf[A]

  override def updated[B >: A](index: Int, elem: B): NVector[B] = {
    if(index < 0 || index >= length) throw new IndexOutOfBoundsException
    if(index >= len12) {
      val io = index - len12
      val i3 = io >> BITS2
      val i2 = (io >> BITS) & MASK
      val i1 = io & MASK
      if(i3 < data.length)
        new NVector3(prefix1, len1, prefix2, len12, copyUpdate(data, i3, i2, i1, elem), suffix2, suffix1, length)
      else if(i2 < suffix2.length)
        new NVector3(prefix1, len1, prefix2, len12, data, copyUpdate(suffix2, i2, i1, elem), suffix1, length)
      else
        new NVector3(prefix1, len1, prefix2, len12, data, suffix2, copyUpdate(suffix1, i1, elem), length)
    } else if(index >= len1) {
      val ip = index - len1
      new NVector3(prefix1, len1, copyUpdate(prefix2, ip >> BITS, ip & MASK, elem), len12, data, suffix2, suffix1, length)
    } else {
      new NVector3(copyUpdate(prefix1, index, elem), len1, prefix2, len12, data, suffix2, suffix1, length)
    }
  }

  override def appended[B >: A](elem: B): NVector[B] = {
    if(suffix1.length < WIDTH)
      new NVector3(prefix1, len1, prefix2, len12, data, suffix2, copyAppend(suffix1, elem), length+1)
    else if(suffix2.length < WIDTH)
      new NVector3(prefix1, len1, prefix2, len12, data, copyAppend(suffix2, suffix1), Array[Any](elem), length+1)
    else if(data.length < WIDTH)
      new NVector3(prefix1, len1, prefix2, len12, copyAppend(data, suffix2), Array[Array[Any]](suffix1), Array[Any](elem), length+1)
    else ???
  }

  override def prepended[B >: A](elem: B): NVector[B] = {
    if(len1 < WIDTH)
      new NVector3(copyPrepend(elem, prefix1), len1+1, prefix2, len12+1, data, suffix2, suffix1, length+1)
    else if(len12 < WIDTH2)
      new NVector3(Array[Any](elem), 1, copyPrepend(prefix1, prefix2), len12+1, data, suffix2, suffix1, length+1)
    else if(length < WIDTH3)
      new NVector3(Array[Any](elem), 1, Array[Array[Any]](prefix1), len1+1, copyPrepend(prefix2, data), suffix2, suffix1, length+1)
    else ???
  }

  private[collection] def toDebugString: String = {
    val sb = new mutable.StringBuilder()
    sb.append(s"NVector3(len1=$len1, len12=$len12, length=$length)\n")
    logArray(sb, prefix1.asInstanceOf[Array[AnyRef]], "  ", "prefix1: ")
    logArray(sb, prefix2, "  ", "prefix2: ")
    logArray(sb, data, "  ", "data: ")
    logArray(sb, suffix2, "  ", "suffix2: ")
    logArray(sb, suffix1.asInstanceOf[Array[AnyRef]], "  ", "suffix1: ")
    sb.result()
  }

  private[collection] def validate(): Unit = {
    assert(data.length <= (WIDTH-2), s"data.length is ${data.length}, should be <= ${WIDTH-2}")
    assert(data.forall(_ ne null), "data should not contain null entries")
    assert(data.forall(_.forall(_ ne null)), "data children should not contain null entries")
    assert(prefix2.forall(_ ne null), "prefix2 should not contain null entries")
    assert(suffix2.forall(_ ne null), "suffix2 should not contain null entries")
    assert(data.forall(_.length == WIDTH), s"length of all data children should be $WIDTH")
    assert(data.forall(_.forall(_.length == WIDTH)), s"length of all data grandchildren should be $WIDTH")
    assert(prefix2.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all prefix2 children should be > 0 and <= $WIDTH")
    assert(suffix2.forall(a => a.length > 0 && a.length <= WIDTH), s"length of all suffix2 children should be > 0 and <= $WIDTH")
    val prefixlength = prefix2.map(_.length).sum + prefix1.length
    val sum = data.map(_.map(_.length).sum).sum + suffix2.map(_.length).sum + prefixlength + suffix1.length
    assert(sum == length, s"sum of data lengths ($sum) should be vector length ($length)")
    assert(len1 == prefix1.length, s"len1 ($len1) should be prefix1.length (${prefix1.length})")
    assert(len12 == len1 + prefix2.map(_.length).sum, s"len2 ($len12) should be len1 + prefix2.map(_.length).sum (${prefix2.map(_.length).sum})")
    assert(len12 + data.length*WIDTH2 + suffix2.map(_.length).sum + suffix1.length == length,
      s"len12 ($len12) + data.length*WIDTH2 (${data.length}*$WIDTH2) + suffix2.map(_.length).sum (${suffix2.map(_.length).sum}) + suffix1.length (${suffix1.length}) should be length ($length)")
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

  //@inline final def level(len: Int) = ((1 + 6*BITS)-Integer.numberOfLeadingZeros(len-1))/BITS

  @inline final def copyOf(a: Array[Any], len: Int): Array[Any] =
    Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], len).asInstanceOf[Array[Any]]

  @inline final def copyAppend(a: Array[Any], elem: Any): Array[Any] = {
    val ac = Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], a.length).asInstanceOf[Array[Any]]
    ac(ac.length-1) = elem
    ac
  }

  @inline final def copyAppend(a: Array[Array[Any]], elem: Array[Any]): Array[Array[Any]] = {
    val ac = Arrays.copyOf(a, a.length)
    ac(ac.length-1) = elem
    ac
  }

  @inline final def copyAppend(a: Array[Array[Array[Any]]], elem: Array[Array[Any]]): Array[Array[Array[Any]]] = {
    val ac = Arrays.copyOf(a, a.length)
    ac(ac.length-1) = elem
    ac
  }

  @inline final def copyPrepend(elem: Any, a: Array[Any]): Array[Any] = {
    val ac = new Array[Any](a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline final def copyPrepend(elem: Array[Any], a: Array[Array[Any]]): Array[Array[Any]] = {
    val ac = new Array[Array[Any]](a.length+1)
    System.arraycopy(a, 0, ac, 1, a.length)
    ac(0) = elem
    ac
  }

  @inline final def copyPrepend(elem: Array[Array[Any]], a: Array[Array[Array[Any]]]): Array[Array[Array[Any]]] = {
    val ac = new Array[Array[Array[Any]]](a.length+1)
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
    val a1c = a2c(idx2).clone()
    a2c(idx2) = a1c
    a1c(idx1) = elem
    a2c
  }

  @inline final def copyUpdate(a3: Array[Array[Array[Any]]], idx3: Int, idx2: Int, idx1: Int, elem: Any): Array[Array[Array[Any]]] = {
    val a3c = a3.clone()
    val a2c = a3c(idx3).clone()
    a3c(idx3) = a2c
    val a1c = a2c(idx2).clone()
    a2c(idx2) = a1c
    a1c(idx1) = elem
    a3c
  }

  final def logArray[T <: AnyRef](sb: mutable.StringBuilder, a: Array[T], indent: String = "", prefix: String = ""): Unit = {
    def classifier(x: AnyRef): Char =
      if(x eq null) '-'
      else if(x.isInstanceOf[Array[AnyRef]]) 'A'
      else 'o'
    def atos(a: Array[_ <: AnyRef]): String =
      if(a eq null) "-"
      else a.map(classifier).mkString("[", "", "]") + " (" + a.length + ")"
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
