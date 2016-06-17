package multithreading1

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object SimpleExample extends App {
  Future {
    for(i <- 1 to 26) {
      println(i)
      Thread.sleep(10)
    }
  }
  for(c <- 'a' to 'z') {
    println(c)
    Thread.sleep(10)
  }
}