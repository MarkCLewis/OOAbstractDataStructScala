package multithreading1.actorexamples

import akka.actor.Actor

class ChildActor extends Actor {
  import ChildActor._
  def receive = {
    case ToParent =>
      println("Child sending Cascade to parent")
      context.parent ! ParentActor.Cascade
    case SimpleMessage =>
      println("Child Simple = "+self.path)
  }
}

object ChildActor {
  case object ToParent
  case object SimpleMessage
}