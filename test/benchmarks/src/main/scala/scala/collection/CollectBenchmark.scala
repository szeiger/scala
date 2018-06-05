package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class CollectBenchmark {
  @Param(Array("100", "1000"))
  var size: Int = _

  var prefix: String = _
  var suffix: String = _

  var testObject: Iterable[Int] = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    testObject = scala.collection.immutable.ArraySeq.from(0 until 1000)
  }

  @Benchmark def collect1(bh: Blackhole): Any = {
    val res = testObject.collect1 {
      case i if i < size => 1
    }
    bh.consume(res)
  }

  @Benchmark def collect2(bh: Blackhole): Any = {
    val res = testObject.collect2 {
      case i if i < size => 1
    }
    bh.consume(res)
  }

  @Benchmark def collect3(bh: Blackhole): Any = {
    val res = testObject.collect3 {
      case i if i < size => 1
    }
    bh.consume(res)
  }
}
