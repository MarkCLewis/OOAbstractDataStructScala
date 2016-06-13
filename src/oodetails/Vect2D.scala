package oodetails

class Vect2D private(val x: Double, val y: Double) {
  def +(v: Vect2D) = Vect2D(x + v.x, y + v.y)
  def -(v: Vect2D) = Vect2D(x - v.x, y - v.y)
  def *(c: Double) = Vect2D(x * c, y * c)
  def /(c: Double) = Vect2D(x / c, y / c)
  def magnitude = math.sqrt(x * x + y * y)
  def dot(v: Vect2D) = x * v.x + y * v.y
  def cross(v: Vect2D) = x * v.y - y * v.x
  def unary_-() = Vect2D(-x, -y)
  def apply(index:Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"2D vector indexed with $index.")
  }
}

object Vect2D {
  def main(args: Array[String]): Unit = {
    val v1 = Vect2D(1, 2)
    val v2 = Vect2D(2, 2)
    val v3 = -(v1 + v2)
    val v4 = v3 * 3
    println(v3.magnitude+" "+v4.magnitude)
  }
  
  def apply(x:Double, y:Double) = new Vect2D(x, y)
}