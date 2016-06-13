package polymorphism.shape1

class MutableSquare(length: Double) extends MutableRectangle(length, length) {
  /*
  override def width_=(w: Double): Unit = { // ERROR: Can't override mutable variables.
    width = w
    height = w
  }
  override def height_=(h: Double): Unit = { // ERROR: Can't override mutable variables.
    height = h
    width = h
  }
  */
}