/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import scala.collection.generic.CanBuildFrom

/** A trait for all maps upon which operations may be
 *  implemented in parallel.
 *
 *  @define Coll `GenMap`
 *  @define coll general map
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @define mapNote
 *
 *  A map is a collection of bindings from keys to values, where there are
 *  no duplicate keys.
 */
trait GenMapLike[K, +V, +Repr] extends GenIterableLike[(K, V), Repr] with Equals with Parallelizable[(K, V), parallel.ParMap[K, V]] {
  def default(key: K): V
  def get(key: K): Option[V]
  def apply(key: K): V
  def seq: Map[K, V]
  def +[V1 >: V](kv: (K, V1)): GenMap[K, V1]
  def - (key: K): Repr

  // repeated here to override the scaladoc comment
  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
   *  right hand operand. The element type of the $coll is the most specific superclass encompassing
   *  the element types of the two operands.
   *
   *  @param that   the traversable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   *
   *  @usecase def ++[K2, V2](that: GenTraversableOnce[(K2, V2)]): $Coll[K2, V2]
   *    @inheritdoc
   *
   *    Example:
   *    {{{
   *      scala> val m = Map(1-> "a", 2 -> "b")
   *      m: Map[Int,String] = Map(1 -> a, 2 -> b)
   *
   *      scala> val l = List((3, 0))
   *      l: List[(Int, Int)] = List((3,0))
   *
   *      scala> val m2 = m ++ l
   *      m2: Map[Int,Any] = Map(1 -> a, 2 -> b, 3 -> 0)
   *    }}}
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def ++[B >: (K, V), That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  // repeated here to override the scaladoc comment
  /** Builds a new collection by applying a partial function to all elements of this $coll
   *  on which the function is defined.
   *
   *  @param pf     the partial function which filters and maps the $coll.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the partial function
   *                `pf` to each element on which it is defined and collecting the results.
   *                The order of the elements is preserved.
   *
   *  @usecase def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): $Coll[K2, V2]
   *    @inheritdoc
   *
   *    $collectExample
   *
   *    @return       a new $coll resulting from applying the given partial function
   *                  `pf` to each element on which it is defined and collecting the results.
   *                  The order of the elements is preserved.
   */
  def collect[B, That](pf: PartialFunction[(K, V), B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  // repeated here to override the scaladoc comment
  /** Builds a new collection by applying a function to all elements of this $coll
   *  and using the elements of the resulting collections.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   *
   *  @usecase def flatMap[K2, V2](f: ((K, V)) => GenTraversableOnce[(K2, V2)]): $Coll[K2, V2]
   *    @inheritdoc
   *
   *    For example:
   *
   *    {{{
   *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
   *    }}}
   *
   *    The type of the resulting collection is guided by the static type of $coll. This might
   *    cause unexpected results sometimes. For example:
   *
   *    {{{
   *      // lettersOf will return a Seq[Char] of likely repeated letters, instead of a Set
   *      def lettersOf(words: Seq[String]) = words flatMap (word => word.toSet)
   *
   *      // lettersOf will return a Set[Char], not a Seq
   *      def lettersOf(words: Seq[String]) = words.toSet flatMap (word => word.toSeq)
   *
   *      // xs will be an Iterable[Int]
   *      val xs = Map("a" -> List(11,111), "b" -> List(22,222)).flatMap(_._2)
   *
   *      // ys will be a Map[Int, Int]
   *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
   *    }}}
   *
   *    @return       a new $coll resulting from applying the given collection-valued function
   *                  `f` to each element of this $coll and concatenating the results.
   */
  def flatMap[B, That](f: ((K, V)) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  // repeated here to override the scaladoc comment
  /** Builds a new collection by applying a function to all elements of this $coll.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   *
   *  @usecase def map[K2, V2](f: ((K, V)) => (K2, V2)): $Coll[K2, V2]
   *    @inheritdoc
   *    @return       a new $coll resulting from applying the given function
   *                  `f` to each element of this $coll and collecting the results.
   */
  def map[B, That](f: ((K, V)) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  // This hash code must be symmetric in the contents but ought not
  // collide trivially.
  override def hashCode()= scala.util.hashing.MurmurHash3.mapHash(seq)

  /**  Returns the value associated with a key, or a default value if the key is not contained in the map.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the map.
   *   @tparam  B1       the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *   @usecase def getOrElse(key: K, default: => V): V
   *     @inheritdoc
   */
  def getOrElse[V1 >: V](key: K, default: => V1): V1

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def contains(key: K): Boolean

  /** Tests whether this map contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def isDefinedAt(key: K): Boolean

  def keySet: GenSet[K]

  /** Collects all keys of this map in an iterable collection.
   *
   *  @return the keys of this map as an iterable.
   */
  def keys: GenIterable[K]

  /** Collects all values of this map in an iterable collection.
   *
   *  @return the values of this map as an iterable.
   */
  def values: GenIterable[V]

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[K]

  /** Creates an iterator for all values in this map.
   *
   *  @return an iterator over all values that are associated with some key in this map.
   */
  def valuesIterator: Iterator[V]

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  def filterKeys(p: K => Boolean): GenMap[K, V]

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  def mapValues[W](f: V => W): GenMap[K, W]

  /** Compares two maps structurally; i.e., checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     `true` if both maps contain exactly the
   *              same mappings, `false` otherwise.
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenMap[b, _] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) && {
      try {
        this forall {
          case (k, v) => that.get(k.asInstanceOf[b]) match {
            case Some(`v`) =>
              true
            case _ => false
          }
        }
      } catch {
        case ex: ClassCastException => false
      }}
    case _ =>
      false
  }
}
