package multithreading2

object WaitCounting extends App {
  val numThreads = 3
  val threads = Array.tabulate(numThreads)(i => new Thread {
    override def run():Unit = {
      println("Start "+i)
      for (j <- 1 to 5) {
        WaitCounting.synchronized {
          WaitCounting.wait()
          println(i+" : "+j)
          WaitCounting.notify()
        }
      }
    }
  })
  threads.foreach(_.start)
  Thread.sleep(1000)
  println("First notify.")
  synchronized { notify() }
}