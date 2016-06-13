package oodetails

/**
 * A basics 2D mutable vector.
 */
class MutableVect2D_a(private var _x: Double, private var _y: Double) {
  def x = _x
  def y = _y
  def setX(newX: Double): Unit = _x = newX
  def setY(newY: Double): Unit = _y = newY
  def plus(mv: MutableVect2D_a): MutableVect2D_a = {
    _x += mv.x
    _y += mv.y
    this
  }
  def minus(mv: MutableVect2D_a): MutableVect2D_a = {
    _x -= mv.x
    _y -= mv.y
    this
  }
  def scale(c: Double): MutableVect2D_a = {
    _x *= c
    _y *= c
    this
  }
  def magnitude = math.sqrt(x * x + y * y)
}

object MutableVect2D_a {
  def main(args:Array[String]):Unit = {
    val v1 = new MutableVect2D_a(1, 2)
    val v2 = new MutableVect2D_a(2, 2)
    v1.plus(v2)
    println(v1.magnitude)
  }
}