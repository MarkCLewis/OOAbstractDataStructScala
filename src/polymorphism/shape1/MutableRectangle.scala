package polymorphism.shape1

import scalafx.scene.canvas.GraphicsContext

class MutableRectangle(var width: Double, var height: Double) extends Shape {
  override def area: Double = width * height
  override def perimeter: Double = 2.0 * (width + height)
  override def draw(gc: GraphicsContext): Unit = {
    gc.fillRect(0.0, 0.0, width, height)
  }
}