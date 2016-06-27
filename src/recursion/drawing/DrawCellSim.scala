package recursion.drawing

import scala.collection.mutable
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.layout.VBox
import priorityqueues.adt.SortedListPriorityQueue

class DrawCellSim(
    d: Drawing,
    private var _width: Double,
    private var _height: Double,
    private var _maximumPopulation: Int,
    private var _minSplitTime: Double,
    private var _maxSplitTime: Double,
    private var _numSims: Int) extends Drawable(d) {

  @transient private var propPanel: Node = null
  @transient private var allSplits = Vector[mutable.Buffer[Double]]()

  runAllSims()

  override def toString = "Cell Simulation"

  def draw(gc: GraphicsContext): Unit = {
    if (allSplits == null) runAllSims()
    gc.stroke = Color.Black
    val maxTime = allSplits.map(_.last).max
    for (s <- allSplits) {
      var (lastX, lastY) = (0.0, _height - _height / _maximumPopulation)
      for (i <- s.indices) {
        val (x, y) = (s(i) * _width / maxTime, _height - (i + 1) * _height / _maximumPopulation)
        gc.strokeLine(lastX, lastY, x, y)
        lastX = x
        lastY = y
      }
    }
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      val panel = new VBox
      val widthField = DrawingMain.labeledTextField("Width", _width.toString, s => { _width = s.toDouble; runAllSims() })
      val heightField = DrawingMain.labeledTextField("Height", _height.toString, s => { _height = s.toDouble; runAllSims() })
      val maxPopField = DrawingMain.labeledTextField("Maximum Pop", _maximumPopulation.toString, s => { _maximumPopulation = s.toInt; runAllSims() })
      val minSplitField = DrawingMain.labeledTextField("Min Split Time", _minSplitTime.toString, s => { _minSplitTime = s.toDouble; runAllSims() })
      val maxSplitField = DrawingMain.labeledTextField("Max Split Time", _maxSplitTime.toString, s => { _maxSplitTime = s.toDouble; runAllSims() })
      val numSimsField = DrawingMain.labeledTextField("Num Sims", _numSims.toString, s => { _numSims = s.toInt; runAllSims() })
      panel.children = List(widthField, heightField, maxPopField, minSplitField, maxSplitField, numSimsField)
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="cellSim" width={ _width.toString() } height={ _height.toString() } maxPop={ _maximumPopulation.toString() } minSplit={ _minSplitTime.toString() } maxSplit={ _maxSplitTime.toString() } numSims={ _numSims.toString() }>
    </drawable>
  }

  private def runAllSims(): Unit = {
    allSplits = Vector.fill(_numSims)(runSimulation())
    drawing.draw()
  }

  private def runSimulation(): mutable.Buffer[Double] = {
    val splits = mutable.Buffer[Double]()
    val pq = new SortedListPriorityQueue[Double]((a, b) => b.compareTo(a))
    pq.enqueue(_minSplitTime + math.random * (_maxSplitTime - _minSplitTime))
    while (splits.length + 1 < _maximumPopulation) {
      val time = pq.dequeue()
      splits += time
      pq.enqueue(time + _minSplitTime + math.random * (_maxSplitTime - _minSplitTime))
      pq.enqueue(time + _minSplitTime + math.random * (_maxSplitTime - _minSplitTime))
    }
    splits
  }
}

object DrawCellSim {
  def apply(d: Drawing, width: Double, height: Double, maximumPopulation: Int, minSplitTime: Double, maxSplitTime: Double, numSims: Int): DrawCellSim = {
    new DrawCellSim(d, width, height, maximumPopulation, minSplitTime, maxSplitTime, numSims)
  }

  def apply(n: xml.Node, d: Drawing): DrawCellSim = {
    val width = (n \ "@width").text.toDouble
    val height = (n \ "@height").text.toDouble
    val maximumPopulation = (n \ "@maxPop").text.toInt
    val minSplitTime = (n \ "@minSplit").text.toDouble
    val maxSplitTime = (n \ "@maxSplit").text.toDouble
    val numSims = (n \ "@numSims").text.toInt
    new DrawCellSim(d, width, height, maximumPopulation, minSplitTime, maxSplitTime, numSims)
  }
}