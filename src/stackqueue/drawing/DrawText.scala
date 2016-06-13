package stackqueue.drawing

import scalafx.Includes._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.layout.VBox
import scalafx.scene.control.ColorPicker
import scalafx.event.ActionEvent

class DrawText(
    d: Drawing,
    private var _x: Double,
    private var _y: Double,
    private var _text: String,
    private var _color: Color) extends Drawable(d) {

  private var propPanel: Option[Node] = None

  override def toString = "Text: "+_text

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = _color
    gc.fillText(_text, _x, _y)
  }

  def propertiesPanel(): Node = {
    if (propPanel.isEmpty) {
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
      propPanel = Some(panel)
    }
    propPanel.get
  }
}