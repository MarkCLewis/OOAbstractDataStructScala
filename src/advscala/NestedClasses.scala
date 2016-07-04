package advscala

class NestedClasses(x: Double, y: Double) {
  val root: Node = new Plus(new Num(x), new Num(y))

  trait Node {
    def eval: Double
  }

  class Plus(left: Node, right: Node) extends Node {
    def eval = left.eval + right.eval
  }

  class Num(n: Double) extends Node {
    def eval = n
  }
}

object NestedClass {
  def main(args: Array[String]) {
    val a = new NestedClasses(4, 5)
    val b = new NestedClasses(8.7, 9.3)

    a.root match {
      case n: a.Num => println("A number")
      case n: a.Node => println("Not a number")
    }

    def evalNode(n: NestedClasses#Node) = n.eval
    def evalNodeA(n: a.Node) = n.eval
    def evalNodeB(n: b.Node) = n.eval

    println(evalNode(a.root))
    println(evalNodeA(a.root))
    //    println(evalNodeB(a.root)) // This is a type mismatch.
    println(evalNode(b.root))
    //    println(evalNodeA(b.root)) // This is a type mismatch.
    println(evalNodeB(b.root))
  }
}
