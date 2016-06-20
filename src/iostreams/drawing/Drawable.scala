package iostreams.drawing

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Node

abstract class Drawable(val drawing: Drawing) extends Serializable {
  def draw(gc: GraphicsContext): Unit
  def propertiesPanel(): Node
}