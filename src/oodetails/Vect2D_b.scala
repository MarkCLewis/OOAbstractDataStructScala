package oodetails

class Vect2D_b(val x: Double, val y: Double) {
  def +(v: Vect2D_b) = new Vect2D_b(x + v.x, y + v.y)
  def -(v: Vect2D_b) = new Vect2D_b(x - v.x, y - v.y)
  def *(c: Double) = new Vect2D_b(x * c, y * c)
  def /(c: Double) = new Vect2D_b(x / c, y / c)
  def magnitude = math.sqrt(x * x + y * y)
  def dot(v: Vect2D_b) = x * v.x + y * v.y
  def cross(v: Vect2D_b) = x * v.y - y * v.x
  def unary_-() = new Vect2D_b(-x, -y)
}

object Vect2D_b {
  def main(args: Array[String]): Unit = {
    val v1 = new Vect2D_b(1, 2)
    val v2 = new Vect2D_b(2, 2)
    val v3 = -(v1 + v2)
    val v4 = v3 * 3
    println(v3.magnitude+" "+v4.magnitude)
  }
}