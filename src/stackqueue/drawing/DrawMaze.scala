package stackqueue.drawing

import collection.mutable
import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import stackqueue.adt.ArrayQueue

class DrawMaze(d: Drawing) extends Drawable(d) {

  private val maze = Array(
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

  private val gridWidth = 20
  private val offsets = Seq((0, -1), (1, 0), (0, 1), (-1, 0))

  private var drawVisited = Set[(Int, Int)]()
  private var startX = 0
  private var startY = 0
  private var endX = 9
  private var endY = 9

  private var propPanel: Option[Node] = None

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
    if (propPanel.isEmpty) {
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
      propPanel = Some(panel)
    }
    propPanel.get
  }

  def breadthFirstShortestPath(): Option[List[(Int, Int)]] = {
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