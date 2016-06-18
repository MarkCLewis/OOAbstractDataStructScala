package multithreading1.actorexamples

import akka.actor.Props
import akka.actor.Actor

class ParentActor extends Actor {
  override def preStart() = {
    context.actorOf(Props[ChildActor], "child1")
    context.actorOf(Props[ChildActor], "child2")
    context.actorOf(Props[ChildActor], "child3")
  }
  
  import ParentActor._
  def receive = {
    case Cascade =>
      println("Parent")
      context.children.foreach(_ ! ChildActor.SimpleMessage)
  }
}

object ParentActor {
  case object Cascade
}