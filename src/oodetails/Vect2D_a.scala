package oodetails

/**
 * A basic 2D immutable vector.
 */
class Vect2D_a(val x: Double, val y: Double) {
  def plus(v: Vect2D_a) = new Vect2D_a(x + v.x, y + v.y)
  def minus(v: Vect2D_a) = new Vect2D_a(x - v.x, y - v.y)
  def scale(c: Double) = new Vect2D_a(x * c, y * c)
  def magnitude = math.sqrt(x * x + y * y)
}

object Vect2D_a {
  def main(args: Array[String]): Unit = {
    val v1 = new Vect2D_a(1, 2)
    val v2 = new Vect2D_a(2, 2)
    val v3 = v1.plus(v2)
    println(v3.magnitude)
  }
}