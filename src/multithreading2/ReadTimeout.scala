package multithreading2

import io.StdIn._

object ReadTimeout extends App {
  val countThread = new Thread {
    override def run: Unit = {
      for (i <- 10 to 1 by -1) {
        println(i)
        Thread.sleep(1000)
      }
      println("Time's up.")
      sys.exit(0)
    }
  }
  println("Enter your age.")
  countThread.start()
  val age = readInt()
  if (age < 18) println("Sorry, you can't come here.")
  else println("Welcome.")
  sys.exit(0)
}