package priorityqueues.drawing

import collection.mutable
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.TreeItem
import scalafx.scene.paint.Color

class Drawing(private[drawing] val vars: mutable.Map[String, Double]) extends Serializable {
  private var root = DrawTransform(this)
  @transient private var gc: GraphicsContext = null

  def graphicsContext_=(g: GraphicsContext): Unit = {
    gc = g
  }

  def graphicsContext = gc

  def draw(): Unit = {
    if (gc != null) {
      gc.fill = Color.White
      gc.fillRect(0, 0, 2000, 2000)
      root.draw(gc)
    }
  }

  def drawTo(tmpGC: GraphicsContext): Unit = {
    tmpGC.fill = Color.White
    tmpGC.fillRect(0, 0, 2000, 2000)
    root.draw(tmpGC)
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

  def toXML: xml.Node = {
    <drawing>
      { root.toXML }
      { for ((k, v) <- vars) yield <var key={ k } value={ v.toString() }/> }
    </drawing>
  }
  
  def advance(dt: Double): Unit = {
    root.advance(dt)
  }
}

object Drawing {
  def apply(): Drawing = {
    new Drawing(mutable.Map())
  }

  def apply(n: xml.Node): Drawing = {
    val vars = mutable.Map((n \ "var").map(vnode => (vnode \ "@key").text -> (vnode \ "@value").text.toDouble): _*)
    val drawing = new Drawing(vars)
    drawing.root = DrawTransform(n \ "drawable", drawing)
    drawing
  }

  def colorToXML(color: Color): xml.Node = {
    <color red={ color.red.toString() } green={ color.green.toString() } blue={ color.blue.toString() } opacity={ color.opacity.toString() }/>
  }

  def xmlToColor(n: xml.NodeSeq): Color = {
    Color((n \ "@red").text.toDouble, (n \ "@green").text.toDouble, (n \ "@blue").text.toDouble, (n \ "@opacity").text.toDouble)
  }
}