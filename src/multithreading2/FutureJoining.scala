package multithreading2

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FutureJoining extends App {
  val futures = for (i <- 1 to 10) yield Future {
    // Do something here
  }
  Future.sequence(futures).foreach(values => {
    // Do other stuff that needs the futures to have finished.
  })
}