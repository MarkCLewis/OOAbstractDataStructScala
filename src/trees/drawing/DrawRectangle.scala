package trees.drawing

import scalafx.Includes._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.layout.VBox
import scalafx.scene.control.ColorPicker
import scalafx.event.ActionEvent
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import trees.util.Formula

class DrawRectangle(
    d: Drawing,
    private var _x: Formula,
    private var _y: Formula,
    private var _width: Formula,
    private var _height: Formula,
    @transient private var _color: Color) extends Drawable(d) {

  @transient private var propPanel: Node = null

  override def toString = "Rectangle"

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = _color
    gc.fillRect(_x(drawing.vars), _y(drawing.vars), _width(drawing.vars), _height(drawing.vars))
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      val panel = new VBox
      val xField = DrawingMain.labeledTextField("x", _x.toString, s => { _x = new Formula(s); drawing.draw() })
      val yField = DrawingMain.labeledTextField("y", _y.toString, s => { _y = new Formula(s); drawing.draw() })
      val widthField = DrawingMain.labeledTextField("width", _width.toString, s => { _width = new Formula(s); drawing.draw() })
      val heightField = DrawingMain.labeledTextField("height", _height.toString, s => { _height = new Formula(s); drawing.draw() })
      val colorPicker = new ColorPicker(_color)
      colorPicker.onAction = (ae: ActionEvent) => {
        _color = colorPicker.value.value
        drawing.draw()
      }
      panel.children = List(xField, yField, widthField, heightField, colorPicker)
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="rectangle" x={ _x.toString() } y={ _y.toString() } width={ _width.toString() } height={ _height.toString() }>
      { Drawing.colorToXML(_color) }
    </drawable>
  }

  private def writeObject(oos: ObjectOutputStream): Unit = {
    oos.defaultWriteObject()
    oos.writeDouble(_color.red)
    oos.writeDouble(_color.green)
    oos.writeDouble(_color.blue)
    oos.writeDouble(_color.opacity)
  }
  private def readObject(ois: ObjectInputStream): Unit = {
    ois.defaultReadObject()
    _color = Color(ois.readDouble(), ois.readDouble(), ois.readDouble(), ois.readDouble())
  }
}

object DrawRectangle {
  def apply(d: Drawing, x: String, y: String, width: String, height: String, color: Color) = {
    new DrawRectangle(d, new Formula(x), new Formula(y), new Formula(width), new Formula(height), color)
  }

  def apply(n: xml.Node, d: Drawing) = {
    val x = (n \ "@x").text
    val y = (n \ "@y").text
    val width = (n \ "@width").text
    val height = (n \ "@height").text
    val color = Drawing.xmlToColor(n \ "color")
    new DrawRectangle(d, new Formula(x), new Formula(y), new Formula(width), new Formula(height), color)
  }
}