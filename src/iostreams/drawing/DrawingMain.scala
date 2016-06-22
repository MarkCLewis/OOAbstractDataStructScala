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
import java.io.IOException
import java.io.FileNotFoundException
import java.io.File
import scalafx.stage.FileChooser
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.BufferedOutputStream
import java.io.OutputStream
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry
import java.io.OutputStreamWriter
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.BufferedInputStream
import java.util.zip.ZipInputStream
import java.io.InputStream

object DrawingMain extends JFXApp {
  private var drawings = List[(Drawing, TreeView[Drawable])]()
  val system = ActorSystem("DrawingSystem")
  val tabPane = new TabPane

  private val creators = Map[String, Drawing => Drawable](
    "Rectangle" -> (drawing => DrawRectangle(drawing, 0, 0, 300, 300, Color.Blue)),
    "Transform" -> (drawing => DrawTransform(drawing)),
    "Text" -> (drawing => DrawText(drawing, 100, 100, "Text", Color.Black)),
    "Maze" -> (drawing => DrawMaze(drawing)),
    "Mandelbrot" -> (drawing => DrawMandelbrot(drawing)),
    "Julia Set" -> (drawing => DrawJulia(drawing)))

  stage = new JFXApp.PrimaryStage {
    title = "Drawing Program"
    scene = new Scene(1000, 700) {
      // Menus
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      val newItem = new MenuItem("New")
      val openItem = new MenuItem("Open")
      val saveBinaryItem = new MenuItem("Save Binary")
      val saveXMLItem = new MenuItem("Save XML")
      val saveZipItem = new MenuItem("Save Zip")
      val closeItem = new MenuItem("Close")
      val exitItem = new MenuItem("Exit")
      fileMenu.items = List(newItem, openItem, saveBinaryItem, saveXMLItem, saveZipItem, closeItem, new SeparatorMenuItem, exitItem)
      val editMenu = new Menu("Edit")
      val addItem = new MenuItem("Add Drawable")
      val copyItem = new MenuItem("Copy")
      val cutItem = new MenuItem("Cut")
      val pasteItem = new MenuItem("Paste")
      editMenu.items = List(addItem, copyItem, cutItem, pasteItem)
      menuBar.menus = List(fileMenu, editMenu)

      // Tabs
      val drawing = Drawing()
      val (tree, tab) = makeDrawingTab(drawing, "Untitled")
      drawings = drawings :+ (drawing, tree)
      tabPane += tab

      // Menu Actions
      newItem.onAction = (ae: ActionEvent) => {
        val drawing = Drawing()
        val (tree, tab) = makeDrawingTab(drawing, "Untitled")
        drawings = drawings :+ (drawing, tree)
        tabPane += tab
        drawing.draw()
      }
      openItem.onAction = (ae: ActionEvent) => open()
      saveBinaryItem.onAction = (ae: ActionEvent) => saveBinary()
      saveXMLItem.onAction = (ae: ActionEvent) => saveXML()
      saveZipItem.onAction = (ae: ActionEvent) => saveZip()
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

      drawing.draw()
      onCloseRequest = (we: WindowEvent) => system.terminate()
    }
  }

  private def makeDrawingTab(drawing: Drawing, tabName: String): (TreeView[Drawable], Tab) = {
    val canvas = new Canvas(2000, 2000)
    val gc = canvas.graphicsContext2D
    drawing.graphicsContext = gc

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
    tab.text = tabName
    tab.content = topSplit
    tab.closable = false
    (drawingTree, tab)
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

  private def open(): Unit = {
    val chooser = new FileChooser()
    val file = chooser.showOpenDialog(stage)
    if (file != null) {
      if (file.getName().endsWith(".bin")) {
        val ois = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)))
        withInputStream(ois)(strm => {
          deserializeDrawingFromStream(strm, file.getName())
        })
      } else if (file.getName().endsWith(".zip")) {
        val zis = new ZipInputStream(new BufferedInputStream(new FileInputStream(file)))
        zis.getNextEntry
        val nd = Drawing(xml.XML.load(zis))
        val (tree, tab) = makeDrawingTab(nd, file.getName)
        drawings = drawings :+ (nd, tree)
        tabPane += tab
        nd.draw()
      } else if (file.getName().endsWith(".xml")) {
        val nd = Drawing(xml.XML.loadFile(file))
        val (tree, tab) = makeDrawingTab(nd, file.getName)
        drawings = drawings :+ (nd, tree)
        tabPane += tab
        nd.draw()
      }
    }
  }

  private def withInputStream[A, B <: InputStream](is: B)(body: B => A): A = {
    try {
      body(is)
    } finally {
      is.close()
    }
  }

  private def deserializeDrawingFromStream(ois: ObjectInputStream, name: String) {
    val obj = ois.readObject()
    obj match {
      case nd: Drawing =>
        val (tree, tab) = makeDrawingTab(nd, name)
        drawings = drawings :+ (nd, tree)
        tabPane += tab
        nd.draw()
      case _ =>
    }
  }

  private def saveBinary(): Unit = {
    withSaveFile(file => {
      val oos = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
      withOutputStream(oos)(strm => {
        setTabAndGetDrawing(file.getName()).foreach(drawing => strm.writeObject(drawing))
      })
    })
  }

  private def saveXML(): Unit = {
    withSaveFile(file => {
      setTabAndGetDrawing(file.getName()).foreach(drawing => xml.XML.save(file.getAbsolutePath(), drawing.toXML))
    })
  }

  private def saveZip(): Unit = {
    withSaveFile(file => {
      setTabAndGetDrawing(file.getName()).foreach(drawing => {
        val zos = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
        zos.putNextEntry(new ZipEntry(file.getName().dropRight(3)+"xml"))
        val sw = new OutputStreamWriter(zos)
        try {
          setTabAndGetDrawing(file.getName()).foreach(drawing => xml.XML.write(sw, drawing.toXML, "", false, null))
        } finally {
          sw.close
        }
      })
    })
  }

  private def withOutputStream[A, B <: OutputStream](os: B)(body: B => A): A = {
    try {
      body(os)
    } finally {
      os.close()
    }
  }

  private def withSaveFile(body: File => Unit): Unit = {
    val chooser = new FileChooser
    val file = chooser.showSaveDialog(stage)
    if (file != null) {
      try {
        body(file)
      } catch {
        case ex: FileNotFoundException => ex.printStackTrace()
        case ex: IOException => ex.printStackTrace()
      }
    }
  }

  private def setTabAndGetDrawing(name: String): Option[Drawing] = {
    val current = tabPane.selectionModel.value.selectedIndex.value
    if (current >= 0) {
      val (drawing, treeView) = drawings(current)
      tabPane.tabs(current).text = name
      Some(drawing)
    } else None
  }

}