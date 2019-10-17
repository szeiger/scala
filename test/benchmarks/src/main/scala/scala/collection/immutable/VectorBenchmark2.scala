package scala.collection.immutable

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
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorBenchmark2 {

  @Param(Array("1", "5", "10", "100", "1000", "2000", "10000" /*, "2000", "1000000" */))
  var size: Int = _

  val rand = new Random(42)
  val o = new AnyRef

  var a: Array[AnyRef] = _
  var v: Vector[AnyRef] = _
  var nv: NVector[AnyRef] = _
  var as: ArraySeq[AnyRef] = _

  @Setup(Level.Trial) def init: Unit = {
    a = Array.fill(size)(o)
    v = Vector.fill(size)(o)
    nv = NVector.fill(size)(o)
    nv.validate()
    as = ArraySeq.fill(size)(o)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Vector
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def vApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(v(i % size))
      i += 1
    }
  }

  @Benchmark def vApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(v(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def vConcatSame(bh: Blackhole): Any = {
    bh.consume(v.concat(v))
  }

  @Benchmark def vPrepend(bh: Blackhole): Any = {
    var coll0, coll = v
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def vAppend(bh: Blackhole): Any = {
    var coll0, coll = v
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
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
    while(i < 1000) {
      v = v.updated(rand.nextInt(size), o)
      i += 1
    }
    bh.consume(v)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ArraySeq
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def asApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(as(i % size))
      i += 1
    }
  }

  @Benchmark def asApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(as(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def asConcatSame(bh: Blackhole): Any = {
    bh.consume(as.concat(as))
  }

  @Benchmark def asPrepend(bh: Blackhole): Any = {
    var coll0, coll = as
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def asAppend(bh: Blackhole): Any = {
    var coll0, coll = as
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
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
    while(i < 1000) {
      bh.consume(a(i % size))
      i += 1
    }
  }

  @Benchmark def aApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(a(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def aConcatSame(bh: Blackhole): Any = {
    bh.consume(a.concat(a))
  }

  @Benchmark def aPrepend(bh: Blackhole): Any = {
    var coll0, coll = a
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def aAppend(bh: Blackhole): Any = {
    var coll0, coll = a
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
      i += 1
    }
    bh.consume(coll)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // NVector
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  @Benchmark def nvApplySequential(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(nv(i % size))
      i += 1
    }
  }

  @Benchmark def nvApplyRandom(bh: Blackhole): Any = {
    var i = 0
    while(i < 1000) {
      bh.consume(nv(rand.nextInt(size)))
      i += 1
    }
  }

  @Benchmark def nvConcatSame(bh: Blackhole): Any = {
    bh.consume(nv.concat(v))
  }

  @Benchmark def nvPrepend(bh: Blackhole): Any = {
    var coll0, coll = nv
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.prepended(o)
      i += 1
    }
    bh.consume(coll)
  }

  @Benchmark def nvAppend(bh: Blackhole): Any = {
    var coll0, coll = nv
    var i = 0
    while(i < size) {
      if(i % 10 == 0) coll = coll0
      coll = coll.appended(o)
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
    while(i < 1000) {
      nv = nv.updated(rand.nextInt(size), o)
      i += 1
    }
    bh.consume(nv)
  }
}
