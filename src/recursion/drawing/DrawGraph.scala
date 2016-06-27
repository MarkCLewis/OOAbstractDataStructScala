package recursion.drawing

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scalafx.Includes._
import scalafx.geometry.Point2D
import scalafx.scene.transform.Transform
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.RadioButton
import scalafx.scene.Node
import scalafx.scene.input.MouseEvent
import scalafx.event.ActionEvent
import scalafx.scene.control.ToggleGroup
import scalafx.scene.layout.VBox
import java.awt.geom.Line2D
import scalafx.application.Platform

class DrawGraph(d: Drawing, nloc: Seq[Point2D], edges: Seq[(Int, Int, Int)], en: Int) extends Drawable(d) with Clickable {
  import DrawGraph.{ GNode, GEdge }

  private val nodes = mutable.Buffer[GNode]()
  for (p <- nloc) nodes += new GNode(p.getX(), p.getY())
  for ((f, t, w) <- edges) nodes(f).edges ::= new GEdge(nodes(f), nodes(t), w)

  private var endNode = if (en < 0) {
    if (nodes.nonEmpty) nodes(0) else null
  } else nodes(en)

  @transient private var propPanel: BorderPane = null
  @transient private var lastTransform: Transform = null
  @transient private var clickAction: MouseEvent => Unit = null
  @transient private var hoverNode: GNode = null
  @transient private var hoverEdge: GEdge = null
  @transient private var startNode: GNode = null
  @transient private var (dx, dy) = (0.0, 0.0)
  @transient private var pathSet: Set[GNode] = null
  private var currentWeight = 1

  def draw(gc: GraphicsContext): Unit = {
    def drawNode(n: GNode): Unit = {
      gc.fill = if (n == endNode) Color.Green else Color.Black
      gc.fillOval(n.x - 5, n.y - 5, 10, 10)
      if (n == hoverNode) {
        gc.fill = Color.Red
        gc.fillOval(n.x - 4, n.y - 4, 8, 8)
      }
    }

    def drawEdge(e: GEdge): Unit = {
      gc.stroke = if (e == hoverEdge) Color.Red else Color.Black
      gc.strokeLine(e.from.x, e.from.y, e.to.x, e.to.y)
      gc.strokeText(e.weight.toString, 0.5f * (e.from.x + e.to.x).toFloat, 0.5f * (e.from.y + e.to.y).toFloat)
    }

    lastTransform = gc.getTransform
    for (n <- nodes) {
      drawNode(n)
      for (e <- n.edges; if (nodes.indexOf(e.from) < nodes.indexOf(e.to))) {
        drawEdge(e)
      }
      gc.stroke = Color.Black
      if (startNode != null) gc.strokeLine(startNode.x, startNode.y, dx, dy)
    }
    if (pathSet != null) {
      gc.fill = Color.Blue
      for (n <- pathSet) {
        gc.fillOval(n.x - 2, n.y - 2, 4, 4)
      }
    }
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      import DrawingMain.showMessageDialog
      propPanel = new BorderPane
      val weight = DrawingMain.labeledTextField("Weight", currentWeight.toString, s => currentWeight = s.toInt)
      val addNodeButton = new RadioButton("Add Node")
      addNodeButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MouseClicked =>
          val p = lastTransform.inverseTransform(new Point2D(e.x, e.y))
          nodes += new GNode(p.x, p.y)
          if (nodes.length == 1) endNode = nodes(0)
        case _ =>
      }
      val addEdgeButton = new RadioButton("Add Edge")
      addEdgeButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MousePressed =>
          if (hoverNode != null) {
            startNode = hoverNode
            dx = startNode.x
            dy = startNode.y
          }
        case MouseEvent.MouseDragged =>
          if (startNode != null) {
            val p = lastTransform.inverseTransform(new Point2D(e.x, e.y))
            dx = p.x
            dy = p.y
          }
        case MouseEvent.MouseReleased =>
          if (startNode != null && hoverNode != null) {
            startNode.edges ::= new GEdge(startNode, hoverNode, currentWeight)
            hoverNode.edges ::= new GEdge(hoverNode, startNode, currentWeight)
          }
          startNode = null
        case _ =>
      }
      val moveNodeButton = new RadioButton("Move Node")
      moveNodeButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MousePressed =>
          if (hoverNode != null) {
            startNode = hoverNode
            dx = startNode.x
            dy = startNode.y
          }
        case MouseEvent.MouseDragged =>
          if (startNode != null) {
            val p = lastTransform.inverseTransform(new Point2D(e.x, e.y))
            startNode.x = p.x
            startNode.y = p.y
            dx = p.x
            dy = p.y
          }
        case MouseEvent.MouseReleased =>
          if (startNode != null && hoverNode != null) {
            val p = lastTransform.inverseTransform(new Point2D(e.x, e.y))
            startNode.x = p.x
            startNode.y = p.y
          }
          startNode = null
        case _ =>
      }
      val removeButton = new RadioButton("Remove")
      removeButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MouseClicked =>
          if (hoverNode != null) {
            nodes -= hoverNode
            for (n <- nodes) n.edges = n.edges.filter(_.to != hoverNode)
            if (hoverNode == endNode && nodes.nonEmpty) endNode = nodes(0)
            pathSet = null
          } else if (hoverEdge != null) {
            hoverEdge.from.edges = hoverEdge.from.edges.filterNot(_ sameAs hoverEdge)
            hoverEdge.to.edges = hoverEdge.to.edges.filterNot(_ sameAs hoverEdge)
          }
        case _ =>
      }
      val setEndButton = new RadioButton("Set End")
      setEndButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MouseClicked =>
          if (hoverNode != null) {
            endNode = hoverNode
          }
        case _ =>
      }
      val reachableButton = new RadioButton("Reachable")
      reachableButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MouseClicked =>
          if (hoverNode != null) Future {
            if (endNode != null) {
              val reachable = canReach(hoverNode, mutable.Set())
              Platform.runLater { showMessageDialog("The end node is"+(if (reachable) "" else " not")+" reachable.") }
            } else Platform.runLater { showMessageDialog("There must be an end node.") }
          }
        case _ =>
      }
      val shortestPathButton = new RadioButton("Shortest Path")
      shortestPathButton.onAction = (ae: ActionEvent) => clickAction = e => e.eventType match {
        case MouseEvent.MouseClicked =>
          if (hoverNode != null) Future {
            if (endNode != null) {
              shortestPath(hoverNode, Set()) match {
                case None =>
                  Platform.runLater { showMessageDialog("There is no path.") }
                case Some((len, ps)) =>
                  Platform.runLater { 
                    showMessageDialog("There is a path of length "+len+".")
                    pathSet = ps
                    drawing.draw()
                  }
              }
            } else Platform.runLater { showMessageDialog("There must be an end node.") }
          }
        case _ =>
      }
      val group = new ToggleGroup
      group.toggles = List(addNodeButton, addEdgeButton, moveNodeButton, removeButton, setEndButton, reachableButton, shortestPathButton)
      val vbox = new VBox
      vbox.children = List(addNodeButton, addEdgeButton, moveNodeButton, removeButton, setEndButton, reachableButton, shortestPathButton, weight)
      propPanel.top = vbox
    }
    propPanel
  }

  override def toString() = "Graph"

  def toXML: xml.Node = {
    <drawable type="graph" en={ nodes.indexOf(endNode).toString }>
      { nodes.map(n => <node x={ n.x.toString } y={ n.y.toString }/>) }
      {
        for (n <- nodes; e <- n.edges) yield <edge from={ nodes.indexOf(e.from).toString } to={ nodes.indexOf(e.to).toString } weight={ e.weight.toString }/>
      }
    </drawable>
  }

  def mouseEvent(me: MouseEvent): Unit = {
    hoverNode = null
    hoverEdge = null
    var lastDist = 1e100
    for (n <- nodes) {
      val p = lastTransform.inverseTransform(new Point2D(me.x, me.y))
      val dx = p.x - n.x
      val dy = p.y - n.y
      val dist = math.sqrt(dx * dx + dy * dy)
      if (dist < 10 && dist < lastDist) {
        hoverNode = n
        lastDist = dist
      }
      if (lastDist > 3) for (e <- n.edges; if (nodes.indexOf(e.from) < nodes.indexOf(e.to))) {
        val line = new Line2D.Double(e.from.x, e.from.y, e.to.x, e.to.y)
        val edist = line.ptSegDist(p.x, p.y).abs
        if (edist < 3 && edist < lastDist) {
          hoverEdge = e
        }
      }
    }
    if (hoverNode != null) hoverEdge = null
    if (clickAction != null) clickAction(me)
    drawing.draw()
  }

  private def canReach(n: GNode, visited: mutable.Set[GNode]): Boolean = {
    if (n == endNode) true
    else if (visited(n)) false
    else {
      visited += n
      n.edges.exists(e => canReach(e.to, visited))
    }
  }

  private def shortestPath(n: GNode, visited: Set[GNode]): Option[(Int, Set[GNode])] = {
    if (n == endNode) Some(0 -> visited)
    else if (visited(n)) None
    else {
      val newVisited = visited + n
      n.edges.foldLeft(None: Option[(Int, Set[GNode])])((last, e) => {
        (last, shortestPath(e.to, newVisited)) match {
          case (None, Some((len, v))) => Some((len + e.weight, v))
          case (_, None) => last
          case (Some((len1, _)), Some((len2, v))) => if (len1 <= len2 + e.weight) last else Some(len2 + e.weight, v)
        }
      })
    }
  }
}

object DrawGraph {
  def apply(d: Drawing) = new DrawGraph(d, List(new Point2D(100, 100)), List(), -1)

  def apply(n: xml.Node, d: Drawing) = {
    val end = (n \ "@en").text.toInt
    val pnts = (n \ "node").map(pn => {
      val x = (pn \ "@x").text.toDouble
      val y = (pn \ "@y").text.toDouble
      new Point2D(x, y)
    })
    val edges = (n \ "edge").map(en => {
      val from = (en \ "@from").text.toInt
      val to = (en \ "@to").text.toInt
      val weight = (en \ "@weight").text.toInt
      (from, to, weight)
    })
    new DrawGraph(d, pnts, edges, end)
  }

  private class GNode(var x: Double, var y: Double) extends Serializable {
    var edges = List[GEdge]()
  }

  private class GEdge(val from: GNode, val to: GNode, val weight: Int) extends Serializable {
    def sameAs(e: GEdge): Boolean = {
      weight == e.weight && ((from == e.from && to == e.to) || (from == e.to && to == e.from))
    }
  }
}