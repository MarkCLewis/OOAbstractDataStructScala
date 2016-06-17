package multithreading1.drawing

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Node

abstract class Drawable(val drawing: Drawing) {
  def draw(gc: GraphicsContext): Unit
  def propertiesPanel(): Node
}