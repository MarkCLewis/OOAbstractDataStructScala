package multithreading1.actorexamples

import akka.actor.Actor
import akka.actor.ActorRef

class CountDownActor extends Actor {
  import CountDownActor._
  def receive = {
    case StartCounting(n, o) =>
      println(n)
      o ! Count(n - 1)
    case Count(n) =>
      if (n > 0) {
        println(n)
        Thread.sleep(1000)
        sender ! Count(n - 1)
      } else {
        context.system.terminate()
      }
  }
}

object CountDownActor {
  case class StartCounting(n: Int, partner: ActorRef)
  case class Count(n: Int)
}
