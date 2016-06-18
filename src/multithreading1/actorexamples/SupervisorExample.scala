package multithreading1.actorexamples

import akka.actor.ActorSystem
import akka.actor.Props

object SupervisorExample extends App {
  val system = ActorSystem("SupervisorExample")
  val actor1 = system.actorOf(Props[LifecycleParent], "parent1")

  val c1 = system.actorSelection("akka://SupervisorExample/user/parent1/child1")
  c1 ! LifecycleChild.Throw
  Thread.sleep(10)
  c1 ! LifecycleChild.SimpleMessage

  Thread.sleep(100)
  system.terminate()
}