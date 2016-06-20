package multithreading2

object ThreadJoining extends App {
  val threads = for(i <- 1 to 10) yield {
    new Thread {
      override def run():Unit = {
        // Do something here
      }
    }
  }
  threads.foreach(_.start())
  
  // This will cause the main thread to pause until the other threads are done.
  threads.foreach(_.join())
  // Do other stuff that needs the threads to have finished.
}