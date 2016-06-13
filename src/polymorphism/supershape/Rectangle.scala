package polymorphism.supershape

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

class Rectangle(val width: Double, val height: Double, c: Color) extends Shape(c) {
  override def area: Double = width * height
  override def perimeter: Double = 2.0 * (width + height)
  override def draw(g: GraphicsContext): Unit = {
    super.draw(g)
    g.fillRect(0.0, 0.0, width, height)
  }
}