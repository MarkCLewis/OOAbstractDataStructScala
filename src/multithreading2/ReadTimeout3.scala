package multithreading2

import io.StdIn._

object ReadTimeout3 extends App {
  var answered = false
  var timeUp = false
  val countThread = new Thread {
    override def run {
      var i = 10
      while (!answered && i > 0) {
        println(i)
        Thread.sleep(1000)
        i -= 1
      }
      if (!answered) {
        println("Time's up.")
        timeUp = true
      }
    }
  }
  println("Enter your age.")
  countThread.start()
  while (!timeUp && !Console.in.ready) {
    Thread.sleep(10)
  }
  if (!timeUp) {
    val age = readInt()
    answered = true
    if (age < 18) println("Sorry, you can't come here.")
    else println("Welcome.")
  }
}