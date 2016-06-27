package recursion.drawing

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.net.ServerSocket
import java.net.Socket
import java.rmi.Naming
import java.rmi.NotBoundException
import java.rmi.RemoteException
import java.rmi.registry.LocateRegistry
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.ZipOutputStream

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import akka.actor.ActorSystem
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonType
import scalafx.scene.control.ChoiceDialog
import scalafx.scene.control.Label
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuBar
import scalafx.scene.control.MenuItem
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.SelectionMode
import scalafx.scene.control.SeparatorMenuItem
import scalafx.scene.control.Slider
import scalafx.scene.control.SplitPane
import scalafx.scene.control.Tab
import scalafx.scene.control.TabPane
import scalafx.scene.control.TextArea
import scalafx.scene.control.TextField
import scalafx.scene.control.TextInputDialog
import scalafx.scene.control.TreeItem
import scalafx.scene.control.TreeView
import scalafx.scene.layout.BorderPane
import scalafx.scene.paint.Color
import scalafx.stage.FileChooser
import scalafx.stage.WindowEvent
import scalafx.scene.control.Button
import scalafx.animation.AnimationTimer
import scalafx.scene.input.MouseEvent

object DrawingMain extends JFXApp {
  try {
    LocateRegistry.createRegistry(1099)
  } catch {
    case ex: Exception =>
      println("Registry port already in use. Hopefully we can use it.")
  }
  private var drawings = List[(Drawing, TreeView[Drawable])]()
  val system = ActorSystem("DrawingSystem")
  val tabPane = new TabPane

  private var server: RemoteCollaborationServer = null
  private var client: Collaborator = null

  private val creators = Map[String, Drawing => Drawable](
    "Rectangle" -> (drawing => DrawRectangle(drawing, 0, 0, 300, 300, Color.Blue)),
    "Transform" -> (drawing => DrawTransform(drawing)),
    "Text" -> (drawing => DrawText(drawing, 100, 100, "Text", Color.Black)),
    "Maze" -> (drawing => DrawMaze(drawing)),
    "Mandelbrot" -> (drawing => DrawMandelbrot(drawing)),
    "Julia Set" -> (drawing => DrawJulia(drawing)),
    "Cell Simulation" -> (drawing => DrawCellSim(drawing, 300, 300, 200, 1.0, 2.0, 10)),
    "Bouncing Balls" -> (drawing => DrawBouncingBalls(drawing)),
    "Graph" -> (drawing => DrawGraph(drawing)))

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
      val collaborateMenu = new Menu("Collaborate")
      val acceptItem = new MenuItem("Accept Drawings")
      val sendItem = new MenuItem("Send Drawing")
      val joinItem = new MenuItem("Join Collaboration")
      val shareItem = new MenuItem("Share Drawing")
      shareItem.disable = true
      collaborateMenu.items = List(acceptItem, sendItem, joinItem, shareItem)
      menuBar.menus = List(fileMenu, editMenu, collaborateMenu)

      // Tabs
      val drawing = Drawing()
      val (tree, tab) = makeDrawingTab(drawing, "Untitled")
      drawings = drawings :+ (drawing, tree)
      tabPane += tab

      // Menu Actions
      newItem.onAction = (ae: ActionEvent) => {
        addDrawing(Drawing(), "Untitled")
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
      acceptItem.onAction = (ae: ActionEvent) => {
        startServer()
        acceptItem.disable = true
      }
      sendItem.onAction = (ae: ActionEvent) => {
        sendDrawing()
      }
      joinItem.onAction = (ae: ActionEvent) => {
        if (server == null) {
          val host = showInputDialog("What server host do you want to use?", "Server Name")
          if (host.nonEmpty && host.get.trim.nonEmpty) {
            try {
              Naming.lookup("rmi://"+host.get+"/Collaborate") match {
                case svr: RemoteCollaborationServer =>
                  server = svr
                  client = new Collaborator(server)
                case _ => throw new NotBoundException("Wrong type found.")
              }
            } catch {
              case (_: NotBoundException) | (_: RemoteException) =>
                val s = new CollaborationServer()
                Naming.bind("Collaborate", s)
                server = s
                client = new Collaborator(server)
            }
          }
        }
        if (client != null) {
          client.stage.show()
          shareItem.disable = false
        }
      }
      shareItem.onAction = (ae: ActionEvent) => {
        val title = showInputDialog("What do you want to call this drawing?", "Drawing Title")
        if (title.nonEmpty) {
          currentTabDrawing.foreach(drawing => server.addDrawing(title.get, drawing))
        }
      }

      // Top Level Setup
      val rootPane = new BorderPane
      rootPane.top = menuBar
      rootPane.center = tabPane
      root = rootPane

      drawing.draw()
      onCloseRequest = (we: WindowEvent) => {
        system.terminate()
        sys.exit(0)
      }
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
    var lastTime = 0L
    val timer = AnimationTimer(time => {
      val delta = (time-lastTime)/1e9
      if(delta > 0 && delta < 0.1) {
        drawing.advance(delta)
        drawing.draw()
      }
      lastTime = time
    })
    val sliderBorder = new BorderPane
    val slider = new Slider(0, 1000, 0)
    val timerButton = new Button("Start Timer")
    timerButton.onAction = (ae:ActionEvent) => {
      if(timerButton.text.value == "Start Timer") {
        timer.start()
        timerButton.text = "Stop Timer"
      } else {
        timer.stop()
        timerButton.text = "Start Timer"
      }
    }
    sliderBorder.center = slider
    sliderBorder.left = timerButton
    val rightTopBorder = new BorderPane
    val canvasScroll = new ScrollPane
    canvasScroll.content = canvas
    rightTopBorder.top = sliderBorder
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
    var currentClickable: Option[Clickable] = None
    drawingTree.selectionModel.value.selectedItem.onChange {
      val selected = drawingTree.selectionModel.value.selectedItem.value
      if (selected != null) {
        propertyPane.content = selected.getValue.propertiesPanel()
        selected.getValue match {
          case c: Clickable => currentClickable = Some(c)
          case _ => currentClickable = None
        }
      } else {
        currentClickable = None
        propertyPane.content = new Label("Nothing selected.")
      }
    }
    
    // Canvas Mouse Interactions
    canvas.onMouseClicked = (me: MouseEvent) => currentClickable.foreach(_.mouseEvent(me))
    canvas.onMousePressed = (me: MouseEvent) => currentClickable.foreach(_.mouseEvent(me))
    canvas.onMouseDragged = (me: MouseEvent) => currentClickable.foreach(_.mouseEvent(me))
    canvas.onMouseReleased = (me: MouseEvent) => currentClickable.foreach(_.mouseEvent(me))
    canvas.onMouseMoved = (me: MouseEvent) => currentClickable.foreach(_.mouseEvent(me))

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

  def addDrawing(nd: Drawing, name: String): Unit = {
    val (tree, tab) = makeDrawingTab(nd, name)
    drawings = drawings :+ (nd, tree)
    tabPane += tab
    nd.draw()
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
        addDrawing(nd, file.getName)
      } else if (file.getName().endsWith(".xml")) {
        val nd = Drawing(xml.XML.loadFile(file))
        addDrawing(nd, file.getName)
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
        addDrawing(nd, name)
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

  private def startServer() {
    Future {
      val ss = new ServerSocket(8080)
      while (true) {
        val sock = ss.accept
        val ois = new ObjectInputStream(new BufferedInputStream(sock.getInputStream()))
        val sender = ois.readUTF()
        val title = ois.readUTF()
        ois.readObject() match {
          case drawing: Drawing => Platform.runLater {
            val response = showConfirmationDialog("Accept "+title+" from "+sender+"?", "Accept Drawing?")
            if (response) {
              addDrawing(drawing, title)
            }
          }
          case _ =>
            println("Got the wrong type.")
        }
        ois.close()
        sock.close()
      }
    }
  }

  private def sendDrawing() {
    currentTabDrawing.foreach { drawing =>
      val host = showInputDialog("What machine do you want to send to?", "Machine Name")
      if (host.nonEmpty) {
        val sock = new Socket(host.get, 8080)
        val oos = new ObjectOutputStream(new BufferedOutputStream(sock.getOutputStream()))
        val name = showInputDialog("Who do you want to say is sending?", "Name")
        if (name.nonEmpty) {
          val title = showInputDialog("What is the title of this drawing?", "Title")
          if (title.nonEmpty) {
            oos.writeUTF(name.get)
            oos.writeUTF(title.get)
            oos.writeObject(drawing)
            oos.close()
            sock.close()
          }
        }
      }
    }
  }

  def showInputDialog(message: String, title: String): Option[String] = {
    val dialog = new TextInputDialog
    dialog.title = title
    dialog.contentText = message
    dialog.showAndWait()
  }

  def showConfirmationDialog(message: String, title: String): Boolean = {
    val alert = new Alert(AlertType.Confirmation)
    alert.title = title
    alert.contentText = message
    alert.showAndWait() == Some(ButtonType.OK)
  }
  
  def showMessageDialog(message: String): Unit = {
    val alert = new Alert(AlertType.Information)
    alert.title = "Message"
    alert.contentText = message
    alert.showAndWait()
  }

  private def currentTabDrawing: Option[Drawing] = {
    val current = tabPane.selectionModel.value.selectedIndex.value
    if (current >= 0) {
      val (drawing, _) = drawings(current)
      Some(drawing)
    } else None
  }
}