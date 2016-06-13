package polymorphism.shapeabstract

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

abstract class Shape(private var color: Color) {
  def area: Double
  def perimeter: Double
  def draw(gc: GraphicsContext): Unit = {
    gc.fill = color
  }
}