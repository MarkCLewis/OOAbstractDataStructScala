package polymorphism.shape1

object ShapeFunctions {
  def areaPerimeterRatio(s: Shape): Double = {
    s.area / s.perimeter
  }

  val circleAPR = areaPerimeterRatio(new Circle(5))
  val rectAPR = areaPerimeterRatio(new Rectangle(4, 5))

  def adjustRectangle(r: MutableRectangle): Unit = {
    r.width = 20
  }

  val square = new MutableSquare(5)
  adjustRectangle(square)

}