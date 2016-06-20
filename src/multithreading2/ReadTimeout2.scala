package multithreading2

import io.StdIn._

object ReadTimeout2 extends App {
  var answered = false
  val countThread = new Thread {
    override def run():Unit = {
      var i = 10
      while (!answered && i > 0) {
        println(i)
        Thread.sleep(1000)
        i -= 1
      }
      if (!answered) {
        println("Time's up.")
        sys.exit(0)
      }
    }
  }
  println("Enter your age.")
  countThread.start()
  val age = readInt()
  answered = true
  if (age < 18) println("Sorry, you can't come here.")
  else println("Welcome.")
}