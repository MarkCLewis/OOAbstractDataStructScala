package multithreading1.actorexamples

import akka.actor._

object SimpleExample extends App {
  class SimpleActor extends Actor {
    def receive = {
      case s: String => println("String "+s)
      case i: Int => println("Int "+i)
    }
  }
  val system = ActorSystem("SimpleExample")
  val actor = system.actorOf(Props[SimpleActor], "FirstActor")
  actor ! "Hi"
  actor ! 42
  actor ! 'a'
  system.terminate
}

