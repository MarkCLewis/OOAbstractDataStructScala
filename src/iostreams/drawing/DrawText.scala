package iostreams.drawing

import scalafx.Includes._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.layout.VBox
import scalafx.scene.control.ColorPicker
import scalafx.event.ActionEvent
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

class DrawText(
    d: Drawing,
    private var _x: Double,
    private var _y: Double,
    private var _text: String,
    @transient private var _color: Color) extends Drawable(d) {

  @transient private var propPanel: Node = null

  override def toString = "Text: "+_text

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = _color
    gc.fillText(_text, _x, _y)
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      val panel = new VBox
      val textField = DrawingMain.labeledTextField("Text", _text, s => { _text = s; drawing.draw() })
      val xField = DrawingMain.labeledTextField("x", _x.toString, s => { _x = s.toDouble; drawing.draw() })
      val yField = DrawingMain.labeledTextField("y", _y.toString, s => { _y = s.toDouble; drawing.draw() })
      val colorPicker = new ColorPicker(_color)
      colorPicker.onAction = (ae: ActionEvent) => {
        _color = colorPicker.value.value
        drawing.draw()
      }
      panel.children = List(textField, xField, yField, colorPicker)
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="text" x={ _x.toString() } y={ _y.toString() }>
      { Drawing.colorToXML(_color) }
      <text>{ _text }</text>
    </drawable>
  }

  private def writeObject(oos: ObjectOutputStream) {
    oos.defaultWriteObject()
    oos.writeDouble(_color.red)
    oos.writeDouble(_color.green)
    oos.writeDouble(_color.blue)
    oos.writeDouble(_color.opacity)
  }
  private def readObject(ois: ObjectInputStream) {
    ois.defaultReadObject()
    _color = Color(ois.readDouble(), ois.readDouble(), ois.readDouble(), ois.readDouble())
  }
}

object DrawText {
  def apply(d: Drawing, x: Double, y: Double, text: String, color: Color) = new DrawText(d, x, y, text, color)

  def apply(n: xml.Node, d: Drawing): DrawText = {
    val x = (n \ "@x").text.toDouble
    val y = (n \ "@y").text.toDouble
    val text = (n \ "text").text
    val color = Drawing.xmlToColor(n \ "color")
    new DrawText(d, x, y, text, color)
  }
}