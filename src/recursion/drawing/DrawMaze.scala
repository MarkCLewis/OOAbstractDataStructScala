package recursion.drawing

import collection.mutable
import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import stackqueue.adt.ArrayQueue
import scalafx.application.Platform
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseButton
import scalafx.scene.transform.Transform
import scalafx.geometry.Point2D

class DrawMaze(
    d: Drawing,
    private val maze: Array[Array[Int]],
    private var startX: Int = 0,
    private var startY: Int = 0,
    private var endX: Int = 9,
    private var endY: Int = 9) extends Drawable(d) with Clickable {

  private val gridWidth = 20
  private val offsets = Seq((0, -1), (1, 0), (0, 1), (-1, 0))

  @transient private var drawVisited = Map[(Int, Int), Color]()

  @transient private var propPanel: Node = null
  @transient private var lastTransform: Transform = null

  override def toString = "Maze"

  def draw(gc: GraphicsContext): Unit = {
    lastTransform = gc.getTransform
    if (drawVisited == null) drawVisited = Map[(Int, Int), Color]()
    gc.fill = Color.Black
    gc.fillRect(-1, -1, 2 + maze(0).length * gridWidth, 2 + maze.length * gridWidth)
    val max = maze.map(_.max).max
    for (j <- maze.indices; i <- maze(j).indices) {
      if (maze(j)(i) == -1) {
        gc.fill = Color.Black
      } else {
        gc.fill = if (drawVisited.contains(i -> j)) drawVisited(i -> j)
        else if (maze(j)(i) < -1) Color.Red
        else if (maze(j)(i) > 0) Color(0.0, 1.0, 1.0, 0.5 + 0.5 * maze(j)(i) / max)
        else Color.White
      }
      gc.fillRect(i * gridWidth, j * gridWidth, gridWidth, gridWidth)
    }
    gc.fill = Color.Green
    gc.fillOval(startX * gridWidth, startY * gridWidth, gridWidth, gridWidth)
    gc.fill = Color.Red
    gc.fillOval(endX * gridWidth, endY * gridWidth, gridWidth, gridWidth)
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      if (drawVisited == null) drawVisited = Map[(Int, Int), Color]()
      val panel = new VBox
      val bfs = new Button("Breadth First Shortest Path")
      bfs.onAction = (ae: ActionEvent) => Future { breadthFirstShortestPath() }
      val dfs = new Button("Depth First Shortest Path")
      dfs.onAction = (ae: ActionEvent) => Future {
        drawVisited = Map()
        DrawingMain.showMessageDialog("The shortest path is "+depthFirstShortestPath(startX, startY)+" steps.")
      }
      val dfsfast = new Button("Fast Depth First Shortest Path")
      dfsfast.onAction = (ae: ActionEvent) => Future {
        drawVisited = Map()
        DrawingMain.showMessageDialog("The shortest path is "+depthFirstShortestPathFast(startX, startY, 0)+" steps.")
        for (j <- maze.indices; i <- maze(j).indices) if (maze(j)(i) > 0) {
          maze(j)(i) = 0
          drawVisited += (i -> j) -> Color.Green
        }
      }
      val longest = new Button("Longest Path")
      longest.onAction = (ae: ActionEvent) => Future {
        drawVisited = Map()
        DrawingMain.showMessageDialog("The shortest path is "+longestPath(startX, startY)+" steps.")
      }
      panel.children = List(bfs, dfs, dfsfast, longest)
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="maze" startX={ startX.toString() } startY={ startY.toString() } endX={ endX.toString() } endY={ endY.toString() }>
      { maze.map(r => <row>{ r.mkString(",") }</row>) }
    </drawable>
  }

  override def mouseEvent(me: MouseEvent): Unit = me.eventType match {
    case MouseEvent.MouseClicked =>
      if (lastTransform != null) {
        val pnt = new Point2D(me.x, me.y)
        val clickPnt = lastTransform.inverseTransform(pnt)
        val gridX = (clickPnt.x / gridWidth).toInt
        val gridY = (clickPnt.y / gridWidth).toInt
        if (gridY >= 0 && gridY < maze.length && gridX >= 0 && gridX < maze(gridY).length) {
          me.button match {
            case MouseButton.Primary => maze(gridY)(gridX) = -1 - maze(gridY)(gridX)
            case MouseButton.Secondary =>
              endX = gridX; endY = gridY
            case MouseButton.Middle =>
              startX = gridX; startY = gridY
            case _ =>
          }
          drawing.draw()
        }
      }
    case _ =>
  }

  private def breadthFirstShortestPath(): Option[List[(Int, Int)]] = {
    val queue = new ArrayQueue[List[(Int, Int)]]
    queue.enqueue(List(startX -> startY))
    var solution: Option[List[(Int, Int)]] = None
    val visited = mutable.Set[(Int, Int)]()
    while (!queue.isEmpty && solution.isEmpty) {
      val steps @ ((x, y) :: _) = queue.dequeue
      for ((dx, dy) <- offsets) {
        val nx = x + dx
        val ny = y + dy
        if (nx >= 0 && nx < maze(0).length && ny >= 0 && ny < maze.length && maze(ny)(nx) == 0 && !visited(nx -> ny)) {
          if (nx == endX && ny == endY) {
            solution = Some((nx -> ny) :: steps)
          }
          visited += nx -> ny
          queue.enqueue((nx -> ny) :: steps)

          // Code for animation
          drawVisited = visited.map(t => t -> Color.Red).toMap
          drawVisited ++= ((nx -> ny) :: steps).map(t => t -> Color.Green)
          Platform.runLater(drawing.draw())
          Thread.sleep(100)
        }
      }
    }
    // Code for animation
    solution.foreach(path => drawVisited = path.map(t => t -> Color.Red).toMap)
    Platform.runLater(drawing.draw())
    Thread.sleep(100)

    solution
  }

  private def depthFirstShortestPath(x: Int, y: Int): Int = {
    if (x == endX && y == endY) 0
    else if (x < 0 || x >= maze.length || y < 0 || y >= maze(x).length || maze(x)(y) < 0) {
      1000000000
    } else {
      maze(x)(y) = -2

      // Code for animation
      Platform.runLater(drawing.draw())
      Thread.sleep(100)

      val ret = 1 + (depthFirstShortestPath(x + 1, y) min
        depthFirstShortestPath(x - 1, y) min
        depthFirstShortestPath(x, y + 1) min
        depthFirstShortestPath(x, y - 1))
      maze(x)(y) = 0
      ret
    }
  }

  private def depthFirstShortestPathFast(x: Int, y: Int, steps: Int): Int = {
    if (x == endX && y == endY) 0
    else if (x < 0 || x >= maze.length || y < 0 || y >= maze(x).length || maze(x)(y) < 0) {
      1000000000
    } else if (maze(x)(y) > 0 && maze(x)(y) <= steps) {
      1000000000
    } else {
      maze(x)(y) = steps

      // Code for animation
      Platform.runLater(drawing.draw())
      Thread.sleep(100)

      val ret = 1 + (depthFirstShortestPathFast(x + 1, y, steps + 1) min
        depthFirstShortestPathFast(x - 1, y, steps + 1) min
        depthFirstShortestPathFast(x, y + 1, steps + 1) min
        depthFirstShortestPathFast(x, y - 1, steps + 1))
      ret
    }
  }

  private def longestPath(x: Int, y: Int): Int = {
    if (x == endX && y == endY) 0
    else if (x < 0 || x >= maze.length || y < 0 || y >= maze(x).length || maze(x)(y) < 0) {
      -1000000000
    } else {
      maze(x)(y) = -2

      // Code for animation
      Platform.runLater(drawing.draw())
      Thread.sleep(100)

      val ret = 1 + (longestPath(x + 1, y) max
        longestPath(x - 1, y) max
        longestPath(x, y + 1) max
        longestPath(x, y - 1))
      maze(x)(y) = 0
      ret
    }
  }
}

object DrawMaze {
  def apply(d: Drawing) = {
    val maze = Array(
      Array(0, -1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, -1, 0, 0, -1, 0, -1, 0, -1, 0),
      Array(0, -1, 0, -1, -1, 0, -1, 0, -1, 0),
      Array(0, -1, 0, 0, -1, 0, -1, 0, -1, 0),
      Array(0, -1, -1, 0, -1, 0, -1, 0, -1, 0),
      Array(0, 0, 0, 0, 0, 0, -1, 0, -1, 0),
      Array(0, -1, -1, -1, -1, -1, -1, 0, -1, -1),
      Array(0, -1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, -1, 0, -1, -1, -1, -1, 0, -1, 0),
      Array(0, 0, 0, -1, 0, 0, 0, 0, -1, 0))
    new DrawMaze(d, maze)
  }

  def apply(n: xml.Node, d: Drawing) = {
    val maze = (n \ "row").map(_.text.split(",").map(_.toInt)).toArray
    val startX = (n \ "@startX").text.toInt
    val startY = (n \ "@startY").text.toInt
    val endX = (n \ "@endX").text.toInt
    val endY = (n \ "@endY").text.toInt
    new DrawMaze(d, maze, startX, startY, endX, endY)
  }
}