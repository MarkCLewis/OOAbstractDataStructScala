package polymorphism.supershape

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

class Shape(private var color: Color) {
  def area: Double = 0.0
  def perimeter: Double = 0.0
  def draw(gc: GraphicsContext): Unit = {
    gc.fill = color
  }
}
