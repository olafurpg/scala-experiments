package experiment

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
// TODO -Xbatch reduces fork-to-fork variance, but incurs 5s -> 30s slowdown
@Fork(value = 10, jvmArgs = Array("-XX:CICompilerCount=2"))
class TestBenchmark {

  @Benchmark
  def defMacroUsage(): String = DefMacroUsage()
}
