package iostreams.drawing

import scalafx.Includes._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.layout.VBox
import scalafx.scene.control.TextField
import scalafx.scene.control.ColorPicker
import scalafx.event.ActionEvent
import scalafx.scene.layout.Priority
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

class DrawRectangle(
    d: Drawing,
    private var _x: Double,
    private var _y: Double,
    private var _width: Double,
    private var _height: Double,
    @transient private var _color: Color) extends Drawable(d) {

  private var propPanel: Option[Node] = None

  override def toString = "Rectangle"

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = _color
    gc.fillRect(_x, _y, _width, _height)
  }

  def propertiesPanel(): Node = {
    if (propPanel.isEmpty) {
      val panel = new VBox
      val xField = DrawingMain.labeledTextField("x", _x.toString, s => { _x = s.toDouble; drawing.draw() })
      val yField = DrawingMain.labeledTextField("y", _y.toString, s => { _y = s.toDouble; drawing.draw() })
      val widthField = DrawingMain.labeledTextField("width", _width.toString, s => { _width = s.toDouble; drawing.draw() })
      val heightField = DrawingMain.labeledTextField("height", _height.toString, s => { _height = s.toDouble; drawing.draw() })
      val colorPicker = new ColorPicker(_color)
      colorPicker.onAction = (ae: ActionEvent) => {
        _color = colorPicker.value.value
        drawing.draw()
      }
      panel.children = List(xField, yField, widthField, heightField, colorPicker)
      propPanel = Some(panel)
    }
    propPanel.get
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