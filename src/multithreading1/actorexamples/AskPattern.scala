package multithreading1.actorexamples

import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.Patterns.ask
import akka.util.Timeout.durationToTimeout

object AskPattern extends App {
  val system = ActorSystem("SimpleExample")
  val bob = system.actorOf(Props(new AskActor("Bob")), "BobActor")
  val jill = system.actorOf(Props(new AskActor("Jill")), "JillActor")
  implicit val ec = system.dispatcher
  
  jill ! AskActor.AskOf(bob)

  val nameFuture = ask(jill, AskActor.AskName, 1.seconds)

  nameFuture.foreach { s =>
      println("name = "+s)
  }

  Thread.sleep(100)
  system.terminate()
}