package stackqueue.drawing

import collection.mutable
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.TreeItem
import scalafx.scene.paint.Color

class Drawing(private var gc: GraphicsContext) {
  private var root = new DrawTransform(this)
  
  private[drawing] val vars = mutable.Map[String, Double]()

  def draw(): Unit = {
    gc.fill = Color.White
    gc.fillRect(0, 0, 2000, 2000)
    root.draw(gc)
  }

  def makeTree: TreeItem[Drawable] = {
    def helper(d: Drawable): TreeItem[Drawable] = d match {
      case dt: DrawTransform =>
        val item = new TreeItem(d)
        item.children = dt.children.map(c => helper(c))
        item
      case _ => new TreeItem(d)
    }
    helper(root)
  }
}