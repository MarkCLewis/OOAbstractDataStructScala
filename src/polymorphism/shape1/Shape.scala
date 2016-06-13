package polymorphism.shape1

import scalafx.scene.canvas.GraphicsContext

class Shape {
  def area: Double = 0.0
  def perimeter: Double = 0.0
  def draw(gc: GraphicsContext): Unit = {}
}

class Rectangle(val width: Double, val height: Double) extends Shape {
  override def area: Double = width * height
  override def perimeter: Double = 2.0 * (width + height)
  override def draw(gc: GraphicsContext): Unit = {
    gc.fillRect(0, 0, width, height)
  }
}

class Circle(val radius: Double) extends Shape {
  override def area: Double = math.Pi * radius * radius
  override def perimeter: Double = 2.0 * math.Pi * radius
  override def draw(gc: GraphicsContext): Unit = {
    gc.fillOval(0, 0, radius, radius)
  }
}

class Square(length: Double) extends Rectangle(length, length)