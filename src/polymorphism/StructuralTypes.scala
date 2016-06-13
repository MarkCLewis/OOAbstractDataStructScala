package polymorphism

object StructuralTypes extends App {
  def structRead[A <: { def hasNextInt(): Boolean; def nextInt(): Int }](source: A): List[Int] = {
    var buf = List[Int]()
    while (source.hasNextInt) buf ::= source.nextInt
    buf.reverse
  }
}