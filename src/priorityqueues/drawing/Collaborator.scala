package priorityqueues.drawing

import java.rmi.RemoteException
import java.rmi.server.UnicastRemoteObject

import scala.collection.mutable

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Button
import scalafx.scene.control.ColorPicker
import scalafx.scene.control.Label
import scalafx.scene.control.RadioButton
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.SplitPane
import scalafx.scene.control.TextArea
import scalafx.scene.control.TextField
import scalafx.scene.control.ToggleGroup
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.FlowPane
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage

@remote trait RemoteCollaborator {
  def post(text: String): Unit
  def requestSketch: Seq[Sketchable]
  def updateSketch(who: RemoteCollaborator, sketch: Seq[Sketchable]): Unit
  def addDrawing(title: String, drawing: Drawing): Unit
}

/**
 * This implementation will run on the clients and bring up a window that will show
 * thumb nails of the shared drawings, a simple sketch pad, and chat window.
 */
class Collaborator(server: RemoteCollaborationServer) extends UnicastRemoteObject with RemoteCollaborator {
  // Set up sketch and drawing variable
  private val sketch = mutable.Buffer[Sketchable]()

  private val (sketches, drawings) = {
    val (cols, draws) = server.joinCollaboration(this)
    mutable.Map(cols.map(c => {
      try {
        Some(c -> c.requestSketch)
      } catch {
        case ex: RemoteException => None
      }
    }).filter(_.nonEmpty).map(_.get): _*) -> mutable.Buffer(draws: _*)
  }

  // Set up chatting variables
  private val nameField = new TextField
  private val chatField = new TextField
  private val chatArea = new TextArea
  chatArea.editable = false
  chatField.onAction = (ae: ActionEvent) => {
    if (chatField.text.value.nonEmpty) {
      val text = nameField.text.value+": "+chatField.text.value
      chatField.text = ""
      foreachCollaborator { _.post(text) }
    }
  }

  // Set up the shared drawing view
  val sharedVBox = new VBox
  sharedVBox.prefWidth = 200
  for ((n, d) <- drawings) drawShared(n, d)

  def drawShared(name: String, drawing: Drawing): Unit = {
    val sharedCanvas = new Canvas(200, 200)
    val sharedGC = sharedCanvas.graphicsContext2D
    sharedGC.fill = Color.White
    sharedGC.fillRect(0, 0, sharedCanvas.width.value, sharedCanvas.height.value)
    sharedGC.scale(0.15, 0.15)
    sharedGC.fill = Color.Black
    drawing.drawTo(sharedGC)
    sharedVBox.children += sharedCanvas
    sharedVBox.children += new Label(name)
    sharedCanvas.onMouseClicked = (me: MouseEvent) => {
      if (me.clickCount == 2) DrawingMain.addDrawing(drawing, name)
    }
  }

  // Set up the main GUI and sketching controls
  val colorPicker = new ColorPicker(Color.Black)
  private var sketchCreator: MouseEvent => Sketchable = (me: MouseEvent) => {
    val c = colorPicker.value.value
    new SketchPath(me.x, me.y, c.red, c.green, c.blue, c.opacity)
  }
  val sketchCanvas = new Canvas(2000, 2000)
  val sketchGC = sketchCanvas.graphicsContext2D
  sketchCanvas.onMousePressed = (me: MouseEvent) => {
    sketch += sketchCreator(me)
  }
  sketchCanvas.onMouseDragged = (me: MouseEvent) => {
    sketch.last.mouseDragged(me.x, me.y)
    redrawSketch()
  }
  sketchCanvas.onMouseReleased = (me: MouseEvent) => {
    sketch.last.mouseReleased(me.x, me.y)
    redrawSketch()
    for ((c, _) <- sketches) {
      c.updateSketch(this, sketch)
    }
  }

  sketchUpdated()
  redrawSketch()

  val stage = new Stage {
    title = "Collabortive Sketch"
    scene = new Scene(800, 600) {
      val chatBorder = new BorderPane
      chatBorder.top = chatField
      val chatScroll = new ScrollPane
      chatScroll.content = chatArea
      chatBorder.center = chatScroll
      val sketchBorder = new BorderPane
      val flowPane = new FlowPane
      val buttonGroup = new ToggleGroup
      val freeformButton = new RadioButton("Freeform")
      freeformButton.selected = true
      freeformButton.onAction = (ae: ActionEvent) => {
        if (freeformButton.selected.value) sketchCreator = (me: MouseEvent) => {
          val c = colorPicker.value.value
          new SketchPath(me.x, me.y, c.red, c.green, c.blue, c.opacity)
        }
      }
      val lineButton = new RadioButton("Line")
      lineButton.onAction = (ae: ActionEvent) => {
        if (lineButton.selected.value) sketchCreator = (me: MouseEvent) => {
          val c = colorPicker.value.value
          new SketchLine(me.x, me.y, c.red, c.green, c.blue, c.opacity)
        }
      }
      val rectButton = new RadioButton("Rectangle")
      rectButton.onAction = (ae: ActionEvent) => {
        if (rectButton.selected.value) sketchCreator = (me: MouseEvent) => {
          val c = colorPicker.value.value
          new SketchRect(me.x, me.y, c.red, c.green, c.blue, c.opacity)
        }
      }
      val ellipseButton = new RadioButton("Ellipse")
      ellipseButton.onAction = (ae: ActionEvent) => {
        if (ellipseButton.selected.value) sketchCreator = (me: MouseEvent) => {
          val c = colorPicker.value.value
          new SketchEllipse(me.x, me.y, c.red, c.green, c.blue, c.opacity)
        }
      }
      val clearButton = new Button("Clear")
      clearButton.onAction = (ae: ActionEvent) => {
        sketch.clear()
        redrawSketch()
        for ((c, _) <- sketches) {
          c.updateSketch(Collaborator.this, sketch)
        }
      }
      flowPane.children = List(freeformButton, lineButton, rectButton, ellipseButton, clearButton, colorPicker)
      buttonGroup.toggles = List(freeformButton, lineButton, rectButton, ellipseButton)
      sketchBorder.top = flowPane
      val sketchScroll = new ScrollPane
      sketchScroll.content = sketchCanvas
      sketchBorder.center = sketchScroll
      val collabSplit = new SplitPane
      collabSplit.orientation = Orientation.Vertical
      collabSplit.items ++= List(sketchBorder, chatBorder)
      collabSplit.dividerPositions = 0.7
      val nameBorder = new BorderPane
      nameBorder.left = new Label("Name")
      nameBorder.center = nameField
      val topBorder = new BorderPane
      topBorder.top = nameBorder
      topBorder.center = collabSplit
      topBorder.left = sharedVBox
      root = topBorder
    }
  }

  // Local helper methods
  private def foreachCollaborator(f: RemoteCollaborator => Unit) {
    for (c <- sketches.keys) try {
      f(c)
    } catch {
      case ex: RemoteException =>
    }
  }

  private def redrawSketch(): Unit = {
    sketchGC.fill = Color.White
    sketchGC.fillRect(0, 0, sketchCanvas.width.value, sketchCanvas.height.value)
    for ((_, s) <- sketches; sk <- s) {
      sk.draw(sketchGC)
    }
    for (sk <- sketch) {
      sk.draw(sketchGC)
    }
  }

  private def sketchUpdated() {
    foreachCollaborator(c => {
      c.updateSketch(this, sketch)
    })
  }

  // Remote methods
  def post(text: String) {
    chatArea.text = chatArea.text.value + text+"\n"
  }

  def requestSketch: Seq[Sketchable] = sketch

  def updateSketch(who: RemoteCollaborator, sketch: Seq[Sketchable]): Unit = {
    sketches(who) = sketch
    redrawSketch()
  }

  def addDrawing(title: String, drawing: Drawing) {
    drawings += title -> drawing
    Platform.runLater { drawShared(title, drawing) }
  }
}