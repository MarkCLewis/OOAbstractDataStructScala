package multithreading1.actorexamples

import akka.actor.ActorSystem
import akka.actor.Props

object HierarchyExample extends App {
  val system = ActorSystem("HierarchyExample")
  val actor1 = system.actorOf(Props[ParentActor],"parent1")
  val actor2 = system.actorOf(Props[ParentActor],"parent2")
  
  val c1 = system.actorSelection("akka://HierarchyExample/user/parent1/child1")
  c1 ! ChildActor.ToParent
  val c2 = system.actorSelection("/user/parent2/child1")
  c2 ! ChildActor.ToParent
  
  Thread.sleep(100)
  system.terminate()
}