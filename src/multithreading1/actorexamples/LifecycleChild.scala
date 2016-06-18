package multithreading1.actorexamples

import akka.actor.Actor

class LifecycleChild extends Actor {
  println("Making a child "+self.path)

  import LifecycleChild._
  def receive = {
    case Throw =>
      println("Child Dying")
      throw new Exception("Something bad happened.")
    case SimpleMessage =>
      println("Child Simple = "+self.path)
  }
  override def preStart() = {
    super.preStart
    println("Prestart")
  }
  override def postStop() = {
    super.postStop
    println("Poststop")
  }
  override def preRestart(reason: Throwable, message: Option[Any]) = {
    super.preRestart(reason, message)
    println("Prerestart "+message)
  }
  override def postRestart(reason: Throwable) = {
    super.postRestart(reason)
    println("Postrestart")
  }
}

object LifecycleChild {
  case object Throw
  case object SimpleMessage
}