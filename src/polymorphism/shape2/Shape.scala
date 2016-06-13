package polymorphism.shape2

import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext

class Shape(private var color: Color) {
  def area: Double = 0.0
  def circumference: Double = 0.0
  def draw(gc: GraphicsContext): Unit = {}
}

class Rectangle(val width:Double,val height:Double,c:Color) extends Shape(c) {
  override def area:Double = width*height
  override def circumference:Double = 2.0*(width+height)
  override def draw(gc: GraphicsContext): Unit = {
//    gc.fill = color  // ERROR: Can't get to the private color data member.
    gc.fillRect(0.0,0.0,width,height)
  }
}

class Circle(val radius:Double,c:Color) extends Shape(c) {
  override def area:Double = math.Pi*radius*radius
  override def circumference:Double = 2.0*math.Pi*radius
  override def draw(gc: GraphicsContext): Unit = {
//    gc.fill = color  // ERROR: Can't get to the private color data member.
    gc.fillOval(0.0,0.0,2.0*radius,2.0*radius)
  }
}