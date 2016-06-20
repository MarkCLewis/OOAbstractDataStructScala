package iostreams.drawing

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.Platform
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuBar
import scalafx.scene.control.MenuItem
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.SeparatorMenuItem
import scalafx.scene.control.Slider
import scalafx.scene.control.SplitPane
import scalafx.scene.control.Tab
import scalafx.scene.control.TabPane
import scalafx.scene.control.TextArea
import scalafx.scene.control.TextField
import scalafx.scene.control.TreeView
import scalafx.scene.layout.BorderPane
import scalafx.scene.paint.Color
import scalafx.event.ActionEvent
import scalafx.scene.control.TreeItem
import scalafx.scene.control.SelectionMode
import scalafx.scene.control.Label
import scalafx.scene.layout.Priority
import scalafx.scene.control.ComboBox
import scalafx.scene.control.Dialog
import scalafx.scene.control.ChoiceDialog
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.stage.WindowEvent
import akka.actor.ActorSystem

object DrawingMain extends JFXApp {
  private var drawings = List[(Drawing, TreeView[Drawable])]()
  val system = ActorSystem("DrawingSystem")

  private val creators = Map[String, Drawing => Drawable](
    "Rectangle" -> (drawing => new DrawRectangle(drawing, 0, 0, 300, 300, Color.Blue)),
    "Transform" -> (drawing => new DrawTransform(drawing)),
    "Text" -> (drawing => new DrawText(drawing, 100, 100, "Text", Color.Black)),
    "Maze" -> (drawing => new DrawMaze(drawing)),
    "Mandelbrot" -> (drawing => new DrawMandelbrot(drawing)),
    "Julia Set" -> (drawing => new DrawJulia(drawing)))

  stage = new JFXApp.PrimaryStage {
    title = "Drawing Program"
    scene = new Scene(1000, 700) {
      // Menus
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      val newItem = new MenuItem("New")
      val openItem = new MenuItem("Open")
      val saveItem = new MenuItem("Save")
      val closeItem = new MenuItem("Close")
      val exitItem = new MenuItem("Exit")
      fileMenu.items = List(newItem, openItem, saveItem, closeItem, new SeparatorMenuItem, exitItem)
      val editMenu = new Menu("Edit")
      val addItem = new MenuItem("Add Drawable")
      val copyItem = new MenuItem("Copy")
      val cutItem = new MenuItem("Cut")
      val pasteItem = new MenuItem("Paste")
      editMenu.items = List(addItem, copyItem, cutItem, pasteItem)
      menuBar.menus = List(fileMenu, editMenu)

      // Tabs
      val tabPane = new TabPane
      val (drawing, tree, tab) = makeDrawingTab()
      drawings = drawings :+ (drawing, tree)
      tabPane += tab

      // Menu Actions
      newItem.onAction = (ae: ActionEvent) => {
        val (drawing, tree, tab) = makeDrawingTab()
        drawings = drawings :+ (drawing, tree)
        tabPane += tab
      }
      closeItem.onAction = (ae: ActionEvent) => {
        val current = tabPane.selectionModel.value.selectedIndex.value
        if (current >= 0) {
          drawings = drawings.patch(current, Nil, 1)
          tabPane.tabs.remove(current)
        }
      }
      addItem.onAction = (ae: ActionEvent) => {
        val current = tabPane.selectionModel.value.selectedIndex.value
        if (current >= 0) {
          val (drawing, treeView) = drawings(current)
          val dtypes = creators.keys.toSeq
          val dialog = new ChoiceDialog(dtypes(0), dtypes)
          val d = dialog.showAndWait() match {
            case Some(s) =>
              val d = creators(s)(drawing)
              val treeSelect = treeView.selectionModel.value.getSelectedItem
              def treeAdd(item: TreeItem[Drawable]): Unit = item.getValue match {
                case dt: DrawTransform =>
                  dt.addChild(d)
                  item.children += new TreeItem(d)
                  drawing.draw()
                case d =>
                  treeAdd(item.getParent)
              }
              if (treeSelect != null) treeAdd(treeSelect)
            case None =>
          }
        }
      }
      exitItem.onAction = (ae: ActionEvent) => {
        // TODO - Add saving before closing down.
        sys.exit()
      }

      // Top Level Setup
      val rootPane = new BorderPane
      rootPane.top = menuBar
      rootPane.center = tabPane
      root = rootPane
      
      onCloseRequest = (we:WindowEvent) => system.terminate()
    }
  }

  private def makeDrawingTab(): (Drawing, TreeView[Drawable], Tab) = {
    val canvas = new Canvas(2000, 2000)
    val gc = canvas.graphicsContext2D
    val drawing = new Drawing(gc)

    // left side
    val drawingTree = new TreeView[Drawable]
    drawingTree.selectionModel.value.selectionMode = SelectionMode.Single
    drawingTree.root = drawing.makeTree
    val treeScroll = new ScrollPane
    drawingTree.prefWidth <== treeScroll.width
    drawingTree.prefHeight <== treeScroll.height
    treeScroll.content = drawingTree
    val propertyPane = new ScrollPane
    val leftSplit = new SplitPane
    leftSplit.orientation = Orientation.Vertical
    leftSplit.items ++= List(treeScroll, propertyPane)

    // right side
    val slider = new Slider(0, 1000, 0)
    val rightTopBorder = new BorderPane
    val canvasScroll = new ScrollPane
    canvasScroll.content = canvas
    rightTopBorder.top = slider
    rightTopBorder.center = canvasScroll
    val commandField = new TextField
    val commandArea = new TextArea
    commandArea.editable = false
    commandField.onAction = (ae: ActionEvent) => {
      val text = commandField.text.value
      if (text.nonEmpty) {
        commandField.text = ""
        val future = Future { Commands(text, drawing) }
        future.foreach(result => Platform.runLater {
          commandArea.text = (commandArea.text+"> "+text+"\n"+result+"\n").value
        })
      }
    }
    val commandScroll = new ScrollPane
    commandArea.prefWidth <== commandScroll.width
    commandArea.prefHeight <== commandScroll.height
    commandScroll.content = commandArea
    val rightBottomBorder = new BorderPane
    rightBottomBorder.top = commandField
    rightBottomBorder.center = commandScroll
    val rightSplit = new SplitPane
    rightSplit.orientation = Orientation.Vertical
    rightSplit.items ++= List(rightTopBorder, rightBottomBorder)
    rightSplit.dividerPositions = 0.7

    // Top Level
    val topSplit = new SplitPane
    topSplit.items ++= List(leftSplit, rightSplit)
    topSplit.dividerPositions = 0.3

    // Tree Selection
    drawingTree.selectionModel.value.selectedItem.onChange {
      val selected = drawingTree.selectionModel.value.selectedItem.value
      if (selected != null) {
        propertyPane.content = selected.getValue.propertiesPanel()
      } else {
        propertyPane.content = new Label("Nothing selected.")
      }
    }

    val tab = new Tab
    tab.text = "Untitled"
    tab.content = topSplit
    tab.closable = false
    (drawing, drawingTree, tab)
  }

  def labeledTextField(labelText: String, initialText: String, action: String => Unit): BorderPane = {
    val bp = new BorderPane
    val label = Label(labelText)
    val field = new TextField
    field.text = initialText
    bp.left = label
    bp.center = field
    field.onAction = (ae: ActionEvent) => action(field.text.value)
    field.focused.onChange(if (!field.focused.value) action(field.text.value))
    bp
  }
}