package polymorphism

trait Person {
  val name: String
}

trait Parent extends Person {
  def children: List[Person]
}

trait Female extends Person

class Mother(val name: String) extends Parent with Female {
  override def children: List[Person] = ???
}