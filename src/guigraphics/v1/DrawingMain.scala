package guigraphics.v1

import scalafx.Includes._
import scalafx.application.JFXApp
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

object DrawingMain extends JFXApp {
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
      tabPane += makeDrawingTab()

      // Top Level Setup
      val rootPane = new BorderPane
      rootPane.top = menuBar
      rootPane.center = tabPane
      root = rootPane
    }
  }

  private def makeDrawingTab(): Tab = {
    // left side
    val drawingTree = new TreeView[String]
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
    val canvas = new Canvas
    rightTopBorder.top = slider
    rightTopBorder.center = canvas
    val commandField = new TextField
    val commandArea = new TextArea
    commandArea.editable = false
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

    val topSplit = new SplitPane
    topSplit.items ++= List(leftSplit, rightSplit)
    topSplit.dividerPositions = 0.3

    val tab = new Tab
    tab.text = "Untitled"
    tab.content = topSplit
    tab
  }
}