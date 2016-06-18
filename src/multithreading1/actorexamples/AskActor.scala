package multithreading1.actorexamples

import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout.durationToTimeout

class AskActor(val name: String) extends Actor {
  implicit val timeout = durationToTimeout(1.seconds)
  implicit val ec = context.system.dispatcher
  import AskActor._
  def receive = {
    case AskName =>
      sender ! name
    case AskOf(o) =>
      (o ? AskName).foreach(n => println(s"They said their name is $n."))
  }
}

object AskActor {
  case object AskName
  case class AskOf(otherActor: ActorRef)
}