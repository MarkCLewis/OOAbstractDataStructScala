package trees.drawing

import scalafx.scene.input.MouseEvent

trait Clickable {
  def mouseEvent(me: MouseEvent):Unit
}