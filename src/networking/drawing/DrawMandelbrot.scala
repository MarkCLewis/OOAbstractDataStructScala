package networking.drawing

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.image.WritableImage
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.application.Platform

class DrawMandelbrot(
    d: Drawing,
    private var rmin: Double,
    private var rmax: Double,
    private var imin: Double,
    private var imax: Double,
    private var width: Int,
    private var height: Int,
    private var maxCount: Int) extends Drawable(d) {
  @transient private var img = new WritableImage(width, height)
  @transient private var propPanel: Node = null

  startDrawing(d.graphicsContext)

  override def toString() = "Mandelbrot"

  def draw(gc: GraphicsContext): Unit = {
    if (img != null) gc.drawImage(img, 0, 0)
    else startDrawing(gc)
  }

  def propertiesPanel(): Node = {
    def checkChangeMade[A](originalValue: A, newValue: => A): A = {
      try {
        val nv = newValue
        if (originalValue == nv) originalValue else {
          startDrawing(drawing.graphicsContext)
          nv
        }
      } catch {
        case nfe: NumberFormatException => originalValue
      }
    }
    if (propPanel == null) {
      val panel = new VBox
      panel.children = List(
        DrawingMain.labeledTextField("Real Min", rmin.toString(), s => { rmin = checkChangeMade(rmin, s.toDouble) }),
        DrawingMain.labeledTextField("Real Max", rmax.toString(), s => { rmax = checkChangeMade(rmax, s.toDouble) }),
        DrawingMain.labeledTextField("Imaginary Min", imin.toString(), s => { imin = checkChangeMade(imin, s.toDouble) }),
        DrawingMain.labeledTextField("Imaginary Max", imax.toString(), s => { imax = checkChangeMade(imax, s.toDouble) }),
        DrawingMain.labeledTextField("Max Count", maxCount.toString(), s => { maxCount = checkChangeMade(maxCount, s.toInt) }),
        DrawingMain.labeledTextField("Width", width.toString(), s => { width = checkChangeMade(width, s.toInt) }),
        DrawingMain.labeledTextField("Height", height.toString(), s => { height = checkChangeMade(height, s.toInt) }))
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="mandelbrot" rmin={ rmin.toString() } rmax={ rmax.toString() } imin={ imin.toString() } imax={ imax.toString() } width={ width.toString() } height={ height.toString() } maxCount={ maxCount.toString() }/>
  }

  private def mandelIter(zr: Double, zi: Double, cr: Double, ci: Double) = (zr * zr - zi * zi + cr, 2 * zr * zi + ci)

  private def mandelCount(cr: Double, ci: Double): Int = {
    var ret = 0
    var (zr, zi) = (0.0, 0.0)
    while (ret < maxCount && zr * zr + zi * zi < 4) {
      val (tr, ti) = mandelIter(zr, zi, cr, ci)
      zr = tr
      zi = ti
      ret += 1
    }
    ret
  }

  private def startDrawing(gc:GraphicsContext): Unit = Future {
    val image = new WritableImage(width, height)
    val writer = image.pixelWriter
    for (i <- (0 until width).par) {
      val cr = rmin + i * (rmax - rmin) / width
      for (j <- 0 until height) {
        val ci = imax - j * (imax - imin) / height
        val cnt = mandelCount(cr, ci)
        writer.setColor(i, j, if (cnt == maxCount) Color.Black else
          Color(1.0, 0.0, 0.0, math.log(cnt.toDouble) / math.log(maxCount)))
      }
    }
    Platform.runLater {
      img = image
      drawing.drawTo(gc)
    }
  }
}

object DrawMandelbrot {
  def apply(d: Drawing) = new DrawMandelbrot(d, -1.5, 0.5, -1.0, 1.0, 600, 600, 100)

  def apply(n: xml.Node, d: Drawing) = {
    val rmin = (n \ "@rmin").text.toDouble
    val rmax = (n \ "@rmax").text.toDouble
    val imin = (n \ "@imin").text.toDouble
    val imax = (n \ "@imax").text.toDouble
    val width = (n \ "@width").text.toInt
    val height = (n \ "@height").text.toInt
    val maxCount = (n \ "@maxCount").text.toInt
    new DrawMandelbrot(d, rmin, rmax, imin, imax, width, height, maxCount)
  }
}