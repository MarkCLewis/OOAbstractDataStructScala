package oodetails

class MutableVect2D private(private[this] var _x: Double, private[this] var _y: Double) {
  def x = _x
  def y = _y
  def x_=(newX: Double): Unit = _x = newX
  def y_=(newY: Double): Unit = _y = newY
  def +=(mv: MutableVect2D): MutableVect2D = {
    _x += mv.x
    _y += mv.y
    this
  }
  def -=(mv: MutableVect2D): MutableVect2D = {
    _x -= mv.x
    _y -= mv.y
    this
  }
  def *=(c: Double): MutableVect2D = {
    _x *= c
    _y *= c
    this
  }
  def /=(c: Double): MutableVect2D = {
    _x /= c
    _y /= c
    this
  }
  def magnitude = math.sqrt(x * x + y * y)
  def apply(index:Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"2D vector indexed with $index.")
  }
  def update(index:Int, value:Double): Unit = index match {
    case 0 => x = value
    case 1 => y = value
    case _ => throw new IndexOutOfBoundsException(s"2D vector indexed with $index.")
  }
}

object MutableVect2D {
  def main(args:Array[String]):Unit = {
    val v1 = MutableVect2D(1, 2)
    val v2 = MutableVect2D(2, 2)
    v1 += v2
    println(v1.magnitude)
    v2.x = 3
    v2(0) = 99
  }
  
  def apply(x:Double, y:Double) = new MutableVect2D(x, y)
}