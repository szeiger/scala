package scala.collection

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class HashCollisionBenchmark {
  @Param(Array("10", "100", "1000", "10000"))
  var size: Int = _

  @Param(Array("false", "true"))
  var comparable: Boolean = _

  class Collider(val i: Int) {
    override def hashCode = 0
    override def equals(o: Any) = o match {
      case o: Collider => i == o.i
      case _ => false
    }
  }

  class ComparableCollider(i: Int) extends Collider(i) with Comparable[ComparableCollider] {
    override def compareTo(o: ComparableCollider) = i - o.i
  }

  var data: Array[AnyRef] = _

  @Setup(Level.Trial) def init: Unit = {
    data = (0 until size).map[AnyRef](i => if(comparable) new ComparableCollider(i) else new Collider(i)).toArray
  }

  @Benchmark def champHashMap(bh: Blackhole): Unit = {
    var h = immutable.HashMap.empty[AnyRef, AnyRef]
    data.foreach { o => h = h.updated(o, o) }
    bh.consume(h)
  }

  @Benchmark def champHashSet(bh: Blackhole): Unit = {
    var h = immutable.HashSet.empty[AnyRef]
    data.foreach { o => h = h.incl(o) }
    bh.consume(h)
  }

  @Benchmark def oldHashMap(bh: Blackhole): Unit = {
    var h = immutable.OldHashMap.empty[AnyRef, AnyRef]
    data.foreach { o => h = h.updated(o, o) }
    bh.consume(h)
  }

  @Benchmark def oldHashSet(bh: Blackhole): Unit = {
    var h = immutable.OldHashSet.empty[AnyRef]
    data.foreach { o => h = h.incl(o) }
    bh.consume(h)
  }

  @Benchmark def mutableHashMap(bh: Blackhole): Unit = {
    var h = new mutable.HashMap[AnyRef, AnyRef]
    data.foreach { o => h.update(o, o) }
    bh.consume(h)
  }

  @Benchmark def mutableHashSet(bh: Blackhole): Unit = {
    var h = new mutable.HashSet[AnyRef]
    data.foreach { o => h.add(o) }
    bh.consume(h)
  }

  @Benchmark def javaHashMap(bh: Blackhole): Unit = {
    var h = new java.util.HashMap[AnyRef, AnyRef]
    data.foreach { o => h.put(o, o) }
    bh.consume(h)
  }

  @Benchmark def javaHashSet(bh: Blackhole): Unit = {
    var h = new java.util.HashSet[AnyRef]
    data.foreach { o => h.add(o) }
    bh.consume(h)
  }
}
