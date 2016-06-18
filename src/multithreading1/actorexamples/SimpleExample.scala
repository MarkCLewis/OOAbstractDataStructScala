package multithreading1.actorexamples

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

object SimpleExample extends App {
  class SimpleActor extends Actor {
    private var counter = 0
    
    def receive = {
      case s: String => println("String "+s); count()
      case i: Int => println("Int "+i); count()
    }
    
    def count(): Unit = counter += 1
  }
  val system = ActorSystem("SimpleExample")
  val actor = system.actorOf(Props[SimpleActor], "FirstActor")
  actor ! "Hi"
  actor ! 42
  actor ! 'a'
  system.terminate()
}

