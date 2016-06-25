package networking.drawing

import scala.collection.mutable
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

sealed trait Sketchable extends Serializable {
  def draw(gc: GraphicsContext): Unit
  def mouseDragged(x: Double, y: Double): Unit
  def mouseReleased(x: Double, y: Double): Unit
}

class SketchPath(val sx: Double, val sy: Double, val red: Double, val green: Double, val blue: Double, val opacity: Double) extends Sketchable {
  private val points = mutable.Buffer[(Double, Double)]()
  def draw(gc: GraphicsContext): Unit = {
    gc.stroke = Color(red, green, blue, opacity)
    gc.beginPath()
    gc.moveTo(sx, sy)
    for ((x, y) <- points) gc.lineTo(x, y)
    gc.strokePath()
  }
  def mouseDragged(x: Double, y: Double): Unit = {
    points += (x -> y)
  }
  def mouseReleased(x: Double, y: Double): Unit = {
    points += (x -> y)
  }
}

class SketchLine(val sx: Double, val sy: Double, val red: Double, val green: Double, val blue: Double, val opacity: Double) extends Sketchable {
  private var ex, ey = 0.0
  def draw(gc: GraphicsContext): Unit = {
    gc.stroke = Color(red, green, blue, opacity)
    gc.strokeLine(sx, sy, ex, ey)
  }
  def mouseDragged(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
  def mouseReleased(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
}

class SketchRect(val sx: Double, val sy: Double, val red: Double, val green: Double, val blue: Double, val opacity: Double) extends Sketchable {
  private var (ex, ey) = (sx, sy)
  def draw(gc: GraphicsContext): Unit = {
    gc.fill = Color(red, green, blue, opacity)
    gc.fillRect(sx min ex, sy min ey, (sx - ex).abs, (sy - ey).abs)
  }
  def mouseDragged(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
  def mouseReleased(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
}

class SketchEllipse(val sx: Double, val sy: Double, val red: Double, val green: Double, val blue: Double, val opacity: Double) extends Sketchable {
  private var (ex, ey) = (sx, sy)
  def draw(gc: GraphicsContext): Unit = {
    gc.fill = Color(red, green, blue, opacity)
    gc.fillOval(sx min ex, sy min ey, (sx - ex).abs, (sy - ey).abs)
  }
  def mouseDragged(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
  def mouseReleased(x: Double, y: Double): Unit = {
    ex = x
    ey = y
  }
}
