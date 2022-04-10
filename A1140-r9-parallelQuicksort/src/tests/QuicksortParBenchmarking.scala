package tests
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import sorts._

object QuicksortParBenchmarking extends Bench.OfflineReport {
  /* configuration */
  val sizes = Gen.range("size")(100000, 1000000, 50000)
  val ranges = for {
    size <- sizes
  } yield 0 until size

  val nofSamples = 1
  val nofRuns = 21
  val rand = new scala.util.Random
  var l = new Array[Int](1)
  var nofInits = 0
  def initArray(size: Int) = {
    l = new Array[Int](size)
    for (i <- 0 until size) l(i) = rand.nextInt
  }
  performance of "Sorting" in {
    measure method "algorithms" config (
      exec.benchRuns -> nofRuns,
      exec.independentSamples -> nofSamples) in {

      using(ranges) curve ("java.util.Arrays.sort") setUp {
        r => initArray(r.end)
      } warmUp {
        initArray(1000000)
        java.util.Arrays.sort(l)
      } in {
        r =>
          java.util.Arrays.sort(l)
      }

      using(ranges) curve ("java.util.Arrays.parallelSort") setUp {
        r => initArray(r.end)
      } warmUp {
        initArray(1000000)
        java.util.Arrays.parallelSort(l)
      } in {
        r =>
          java.util.Arrays.parallelSort(l)
      }

      using(ranges) curve ("quicksort.sort") setUp {
        r => initArray(r.end)
      } warmUp {
        initArray(1000000)
        quicksort.sort(l)
      } in {
        r => quicksort.sort(l)
      }

      using(ranges) curve ("quicksort.sortPar") setUp {
        r => initArray(r.end)
      } warmUp {
        initArray(1000000)
        quicksort.sortPar(l)
      } in {
        r => quicksort.sortPar(l)
      }
    }
  }
}
