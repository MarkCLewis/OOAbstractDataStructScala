package iostreams.drawing

import collection.mutable
import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import stackqueue.adt.ArrayQueue

class DrawMaze(
    d: Drawing,
    private val maze: Array[Array[Int]],
    private var startX: Int = 0,
    private var startY: Int = 0,
    private var endX: Int = 9,
    private var endY: Int = 9) extends Drawable(d) {

  private val gridWidth = 20
  private val offsets = Seq((0, -1), (1, 0), (0, 1), (-1, 0))

  private var drawVisited = Set[(Int, Int)]()

  @transient private var propPanel: Node = null

  override def toString = "Maze"

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = Color.Black
    gc.fillRect(-1, -1, 2 + maze(0).length * gridWidth, 2 + maze.length * gridWidth)
    for (j <- maze.indices; i <- maze(j).indices) {
      if (maze(j)(i) == 0) {
        gc.fill = if (drawVisited(i -> j)) Color.Green else Color.White
      } else {
        gc.fill = Color.Black
      }
      gc.fillRect(i * gridWidth, j * gridWidth, gridWidth, gridWidth)
    }
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      val panel = new VBox
      val bfs = new Button("Breadth First Search")
      bfs.onAction = (ae: ActionEvent) => {
        breadthFirstShortestPath() match {
          case None => drawVisited = Set()
          case Some(lst) => drawVisited = lst.toSet
        }
        drawing.draw()
      }
      panel.children = List(bfs)
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="maze" startX={ startX.toString() } startY={ startY.toString() } endX={ endX.toString() } endY={ endY.toString() }>
      { maze.map(r => <row>{ r.mkString(",") }</row>) }
    </drawable>
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
        }
      }
    }
    solution
  }

}

object DrawMaze {
  def apply(d: Drawing) = {
    val maze = Array(
      Array(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 0, 1, 0, 1, 0, 1, 0),
      Array(0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
      Array(0, 1, 0, 0, 1, 0, 1, 0, 1, 0),
      Array(0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
      Array(0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      Array(0, 1, 1, 1, 1, 1, 1, 0, 1, 1),
      Array(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 1, 1, 1, 1, 0, 1, 0),
      Array(0, 0, 0, 1, 0, 0, 0, 0, 1, 0))
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