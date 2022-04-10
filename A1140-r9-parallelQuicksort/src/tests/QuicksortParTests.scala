package tests
import org.scalatest._

import scala.util.Random
import sorts._

class QuicksortParSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random

  def randArrayInt(n: Int): Array[Int] = {
    require(n > 0)
    Array.tabulate[Int](n)(j => rand.nextInt)
  }

  //println("P = "+par.getParallelism)

  "The quicksort.sort method" should "work correctly" in {
    val nofTests = 100
    val N = 10000
    for(t <- 1 to nofTests) {
      val a = randArrayInt(N)
      val sorted = a.sorted
      quicksort.sort(a)
      for(i <- 0 until N) {a(i) should be (sorted(i))}
    }
  }
  "The quicksort.sortPar method" should "work correctly" in {
    val nofTests = 100
    val N = 10000
    for(t <- 1 to nofTests) {
      val a = randArrayInt(N)
      val sorted = a.sorted
      quicksort.sortPar(a)
      for(i <- 0 until N) {a(i) should be (sorted(i))}
    }
  }
  "The quicksort.sortPar method" should "parallelize" in {
    val nofTests = 5
    val N = 5000000
    println("Testing whether quicksort.sortPar parallelizes")
    for(t <- 1 to nofTests) {
      println(s" Test $t")
      val a = randArrayInt(N)
      val b = a.clone
      val c = a.clone
      val (seqResult,seqCPU, seqWC) = timer.measureTimes {quicksort.sort(a) }
      println(f"  Sequential times: CPU $seqCPU%.3g, wall clock $seqWC%.3g")
      val (wcResult,wcTime) = timer.measureWallClockTime {quicksort.sortPar(b) }
      println(f"  Parallel algorithm wall clock time: $wcTime%.3g")
      println(f"  Speedup: ${seqWC / wcTime}%.3g")
      val (r,work,span) = par.measure {quicksort.sortPar(c) }
      println(f"  Approximate parallel work=$work%.3g, span=$span%.3g, calls to par.parallel=${par.nofParallelCalls}")
      for(i <- 0 until N) {a(i) should be (c(i))}
      span should be <= (seqCPU / 4.0)
    }
  }
}

