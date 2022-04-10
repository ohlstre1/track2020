package sorts

import java.lang.management.{ ManagementFactory, ThreadMXBean }

object par {
  import java.util.concurrent.{ForkJoinTask,RecursiveTask,ForkJoinPool,ForkJoinWorkerThread}
  import collection.JavaConverters._

  val forkJoinPool = new ForkJoinPool()
  def getParallelism: Int = forkJoinPool.getParallelism()

  val bean: ThreadMXBean = ManagementFactory.getThreadMXBean()
  def getCpuTime: Long = if (bean.isCurrentThreadCpuTimeSupported()) bean.getCurrentThreadCpuTime() else 0L

  class ThreadStruct {
    var startTime = getCpuTime
    var work = 0L
    var span = 0L
  }
  val threadStructs = new scala.collection.concurrent.TrieMap[Thread, ThreadStruct]()

  private var inMeasure = false
  var _nofParallelCalls = 0L
  def nofParallelCalls = _nofParallelCalls

  // Taking the CPU time is not cheap, compute a compensation time
  private val compensationTrials = 1000000
  private val compensation: Long = {
    var i = 0
    var s = getCpuTime
    var e = s
    // Warmup
    while(i < compensationTrials) {
      e = getCpuTime
      i += 1
    }
    // Actual measurement
    s = getCpuTime
    e = s
    i = 0
    while(i < compensationTrials) {
      e = getCpuTime
      i += 1
    }
    (e-s)/compensationTrials
  }

  /**
   * Execute the code and measure (an estimate of) its parallel work and span.
   * The code may not call mmeasure again and parallel execution of measure is
   * not allowed.
   */
  def measure[A](code: => A): (A, Double, Double) = {
    require(!inMeasure, "Nested or parallel measures not allowed")
    inMeasure = true
    _nofParallelCalls = 0L
    threadStructs.clear()
    val s = new ThreadStruct()
    threadStructs(Thread.currentThread) = s
    s.startTime = getCpuTime
    val result = code
    val dur = getCpuTime - s.startTime
    assert(threadStructs(Thread.currentThread) == s)
    s.work += dur
    s.span += dur
    inMeasure = false
    (result, s.work/1e9, s.span/1e9)
  }

  /**
   * Execute the tasks taskA and taskB in parallel and return their results.
   */
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    if(inMeasure) parallelMeasure(taskA, taskB)
    else parallelReal(taskA, taskB)

  private def parallelMeasure[A, B](taskA: => A, taskB: => B): (A, B) = {
    assert(inMeasure)
    _nofParallelCalls += 1

    val tAtStart = getCpuTime
    val s = threadStructs(Thread.currentThread)
    val durAtStart = tAtStart - s.startTime - compensation
    s.work += durAtStart
    s.span += durAtStart
    
    val s2 = new ThreadStruct()
    threadStructs(Thread.currentThread) = s2
    s2.startTime = getCpuTime
    val leftResult = taskA
    var leftWork = 0L
    var leftSpan = 0L
    val durA = getCpuTime - s2.startTime - compensation
    assert(threadStructs(Thread.currentThread) == s2)
    leftWork = s2.work + durA
    leftSpan = s2.span + durA

    val s3 = new ThreadStruct()
    threadStructs(Thread.currentThread) = s3
    s3.startTime = getCpuTime
    val rightResult = taskB
    var rightWork = 0L
    var rightSpan = 0L
    val durB = getCpuTime - s3.startTime - compensation
    assert(threadStructs(Thread.currentThread) == s3)
    rightWork = s3.work + durB
    rightSpan = s3.span + durB

    threadStructs(Thread.currentThread) = s
    s.work += (leftWork + rightWork)
    s.span += (leftSpan max rightSpan)
    s.startTime = getCpuTime
    (leftResult, rightResult)
  }

  private def parallelReal[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task(taskB)
    val left = taskA
    (left, right.join())
  }

  private def task[T](body: => T): ForkJoinTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = {
        body
      }
    }
    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    }
    t
  }

}
