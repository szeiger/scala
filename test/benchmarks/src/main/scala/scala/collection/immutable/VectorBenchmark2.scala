package scala.collection.immutable

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import java.util.Arrays

import scala.collection.IterableOps

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorBenchmark2 {

  //@Param(Array("1", "5", "10", "100", "1000", "2000", "10000", "50000", "500000", "5000000", "50000000"))
  @Param(Array("1", "10", "100", "1000", "10000", "50000"))
  //@Param(Array("1", "5", "10", "100", "1000", "2000", "10000"))
  //@Param(Array("1", "5", "10"))
  //@Param(Array("2000", "10000"))
  //@Param(Array("100", "500", "1000"))
  var size: Int = _

  val rand = new Random(42)
  val o, p = new AnyRef

  var a: Array[AnyRef] = _
  var v: Vector[AnyRef] = _
  var nv: NVector[AnyRef] = _
  var as: ArraySeq[AnyRef] = _

  @Setup(Level.Trial) def init: Unit = {
    //a = Array.fill(size)(o)
    v = Vector.fill(size)(o)
    nv = NVector.fill(size)(o)
    //NVector.fillSparse(size)(o)
    as = ArraySeq.fill(size)(o)
    //println(s"init: size = $size, JVM: " + ManagementFactory.getRuntimeMXBean().getName())
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Vector
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def vApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(v(i % size))
      i += 1
    }
  }

  @Benchmark def vApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(v(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def vPrepend(bh: Blackhole): Any = {
    var coll0, coll = v
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vAppend(bh: Blackhole): Any = {
    var coll0, coll = v
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vApprepend(bh: Blackhole): Any = {
    var coll0, coll = v
    var i = 0
    while(i < size) {
      if(i % 2 == 0) coll = coll.appended(o)
      else coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vBuild(bh: Blackhole): Any = {
    val b = Vector.newBuilder[AnyRef]
    var i = 0
    while(i < size) {
      b.addOne(o)
      i += 1
    }
    bh.consume(b.result())
  }

  @Benchmark def vUpdateSequential(bh: Blackhole): Any = {
    var v = this.v
    var i = 0
    while(i < 1000) {
      v = v.updated(i % size, o)
      i += 1
    }
    bh.consume(v)
  }

  @Benchmark def vUpdateRandom(bh: Blackhole): Any = {
    var v = this.v
    var i = 0
    while(i < 100) {
      v = v.updated(rand.nextInt(size), o)
      i += 1
    }
    bh.consume(v)
  }

  @Benchmark def vHead(bh: Blackhole): Any = {
    var coll = v
    var i = 0
    while(i < 1000) {
      bh.consume(coll.head)
      i += 1
    }
  }

  @Benchmark def vLast(bh: Blackhole): Any = {
    var coll = v
    var i = 0
    while(i < 1000) {
      bh.consume(coll.last)
      i += 1
    }
  }

  @Benchmark def vTail(bh: Blackhole): Any = {
    var coll, coll1 = v
    var i = 0
    while(i < 1000) {
      coll = coll.tail
      bh.consume(coll)
      if(coll.isEmpty) coll = coll1
      i += 1
    }
  }

  @Benchmark def vIterator(bh: Blackhole): Any = {
    var coll = v
    val it = coll.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def vForeach(bh: Blackhole): Any = {
    var coll = v
    coll.foreach(bh.consume _)
  }

  @Benchmark def vMapIdentity(bh: Blackhole): Any = {
    var coll = v
    bh.consume(coll.map(identity))
  }

  @Benchmark def vMapNew(bh: Blackhole): Any = {
    var coll = v
    bh.consume(coll.map(_ => p))
  }

  @Benchmark def vBulkAppend2(bh: Blackhole): Any = {
    var coll = v
    val coll1 = Vector.fill(2)(o)
    var i = 0
    while(i < 100) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vBulkAppend10p(bh: Blackhole): Any = {
    var coll = v
    val coll1 = as.take(coll.size/10)
    var i = 0
    while(i < 100) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vBulkAppend100p(bh: Blackhole): Any = {
    var coll = v
    val coll1 = as
    var i = 0
    while(i < 10) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vBulkAppendSame(bh: Blackhole): Any = {
    var coll = v
    val coll1 = v
    var i = 0
    while(i < 10) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  /*
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ArraySeq
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def asApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(as(i % size))
      i += 1
    }
  }

  @Benchmark def asApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(as(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def asPrepend(bh: Blackhole): Any = {
    var coll0, coll = as
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def asAppend(bh: Blackhole): Any = {
    var coll0, coll = as
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def asBuild(bh: Blackhole): Any = {
    val b = ArraySeq.newBuilder[AnyRef]
    var i = 0
    while(i < size) {
      b.addOne(o)
      i += 1
    }
    bh.consume(b.result())
  }


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Array
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def aApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(a(i % size))
      i += 1
    }
  }

  @Benchmark def aApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(a(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def aPrepend(bh: Blackhole): Any = {
    var coll0, coll = a
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def aAppend(bh: Blackhole): Any = {
    var coll0, coll = a
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
      i += 1
    }
    bh.consume(coll)
  }
  */

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // NVector
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def nvApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(nv(i % size))
      i += 1
    }
  }

  @Benchmark def nvApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000000) {
      bh.consume(nv(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def nvPrepend(bh: Blackhole): Any = {
    var coll0, coll = nv
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvAppend(bh: Blackhole): Any = {
    var coll0, coll = nv
    var i = 0
    while(i < size) {
      //if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvApprepend(bh: Blackhole): Any = {
    var coll0, coll = nv
    var i = 0
    while(i < size) {
      if(i % 2 == 0) coll = coll.appended(o)
      else coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvBuild(bh: Blackhole): Any = {
    val b = NVector.newBuilder[AnyRef]
    var i = 0
    while(i < size) {
      b.addOne(o)
      i += 1
    }
    bh.consume(b.result())
  }

  @Benchmark def nvFillSparse(bh: Blackhole): Any = {
    bh.consume(NVector.fillSparse(size)(o))
  }

  @Benchmark def nvUpdateSequential(bh: Blackhole): Any = {
    var nv = this.nv
    var i = 0
    while(i < 1000) {
      nv = nv.updated(i % size, o)
      i += 1
    }
    bh.consume(nv)
  }

  @Benchmark def nvUpdateRandom(bh: Blackhole): Any = {
    var nv = this.nv
    var i = 0
    while(i < 100) {
      nv = nv.updated(rand.nextInt(size), o)
      i += 1
    }
    bh.consume(nv)
  }

  @Benchmark def nvHead(bh: Blackhole): Any = {
    var coll = nv
    var i = 0
    while(i < 1000) {
      bh.consume(coll.head)
      i += 1
    }
  }

  @Benchmark def nvLast(bh: Blackhole): Any = {
    var coll = nv
    var i = 0
    while(i < 1000) {
      bh.consume(coll.last)
      i += 1
    }
  }

  @Benchmark def nvTail(bh: Blackhole): Any = {
    var coll, coll1 = nv
    var i = 0
    while(i < 1000) {
      coll = coll.tail
      bh.consume(coll)
      if(coll.isEmpty) coll = coll1
      i += 1
    }
  }

  @Benchmark def nvIterator(bh: Blackhole): Any = {
    var coll = nv
    val it = coll.iterator
    while(it.hasNext) bh.consume(it.next())
  }

  @Benchmark def nvForeach(bh: Blackhole): Any = {
    var coll = nv
    coll.foreach(bh.consume _)
  }

  @Benchmark def nvMapIdentity(bh: Blackhole): Any = {
    var coll = nv
    bh.consume(coll.map(identity))
  }

  @Benchmark def nvMapNew(bh: Blackhole): Any = {
    var coll = nv
    bh.consume(coll.map(_ => p))
  }

  @Benchmark def nvBulkAppend2(bh: Blackhole): Any = {
    var coll = nv
    val coll1 = NVector.fill(2)(o)
    var i = 0
    while(i < 100) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvBulkAppend10p(bh: Blackhole): Any = {
    var coll = nv
    val coll1 = as.take(coll.size/10)
    var i = 0
    while(i < 100) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvBulkAppend100p(bh: Blackhole): Any = {
    var coll = nv
    val coll1 = as
    var i = 0
    while(i < 10) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvBulkAppendSame(bh: Blackhole): Any = {
    var coll = nv
    val coll1 = nv
    var i = 0
    while(i < 10) {
      coll = coll.appendedAll(coll1)
      i += 1
    }
    bh.consume(coll)
  }
}
