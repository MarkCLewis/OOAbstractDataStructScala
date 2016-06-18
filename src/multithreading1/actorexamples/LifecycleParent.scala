package multithreading1.actorexamples

import akka.actor.Props
import akka.actor.Actor
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._

class LifecycleParent extends Actor {
  override def preStart() = {
    context.actorOf(Props[LifecycleChild], "child1")
  }
  def receive = {
    case m =>
      println("Parent")
      context.children.foreach(_ ! LifecycleChild.SimpleMessage)
  }
  override val supervisorStrategy = OneForOneStrategy(loggingEnabled = false) {
    case ex: Exception =>
      println("Child had an exception")
      Restart
  }
}