package multithreading2

object WaitCountingSafe extends App {
  val numThreads = 3
  var handOff = Array.fill(numThreads)(false)
  val threads = Array.tabulate(numThreads)(i => new Thread {
    override def run {
      println("Start "+i)
      for (j <- 1 to 5) {
        WaitCountingSafe.synchronized {
          while (!handOff(i)) {
            WaitCountingSafe.wait()
          }
          handOff(i) = false
          println(i+" : "+j)
          handOff((i + 1) % numThreads) = true
          WaitCountingSafe.notifyAll()
        }
      }
    }
  })
  threads.foreach(_.start)
  Thread.sleep(1000)
  println("First notify.")
  handOff(0) = true
  synchronized { notifyAll() }
}