package multithreading1.actorexamples

import akka.actor.ActorSystem
import akka.actor.Props

object CountDown extends App {
  val system = ActorSystem("CountDownExample")
  val actor1 = system.actorOf(Props[CountDownActor], "Actor1")
  val actor2 = system.actorOf(Props[CountDownActor], "Actor2")

  actor1 ! CountDownActor.StartCounting(10, actor2)
}