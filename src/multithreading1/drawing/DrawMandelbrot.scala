package multithreading1.drawing

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.image.WritableImage
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.application.Platform

class DrawMandelbrot(d: Drawing) extends Drawable(d) {
  private var (rmin, rmax, imin, imax) = (-1.5, 0.5, -1.0, 1.0)
  private var (width, height) = (600, 600)
  private var maxCount = 100
  private var img = new WritableImage(width, height)
  private var propPanel: Option[Node] = None
  
  startDrawing()

  override def toString() = "Mandelbrot"

  def draw(gc: GraphicsContext): Unit = {
    gc.drawImage(img, 0, 0)
  }

  def propertiesPanel(): Node = {
    def checkChangeMade[A](originalValue: A, newValue: => A): A = {
      try {
        val nv = newValue
        if (originalValue == nv) originalValue else {
          startDrawing()
          nv
        }
      } catch {
        case nfe:NumberFormatException => originalValue
      }
    }
    if (propPanel.isEmpty) {
      val panel = new VBox
      panel.children = List(
        DrawingMain.labeledTextField("Real Min", rmin.toString(), s => { rmin = checkChangeMade(rmin, s.toDouble) }),
        DrawingMain.labeledTextField("Real Max", rmax.toString(), s => { rmax = checkChangeMade(rmax, s.toDouble) }),
        DrawingMain.labeledTextField("Imaginary Min", imin.toString(), s => { imin = checkChangeMade(imin, s.toDouble) }),
        DrawingMain.labeledTextField("Imaginary Max", imax.toString(), s => { imax = checkChangeMade(imax, s.toDouble) }),
        DrawingMain.labeledTextField("Max Count", maxCount.toString(), s => { maxCount = checkChangeMade(maxCount, s.toInt) }),
        DrawingMain.labeledTextField("Width", width.toString(), s => { width = checkChangeMade(width, s.toInt) }),
        DrawingMain.labeledTextField("Height", height.toString(), s => { height = checkChangeMade(height, s.toInt) }))
      propPanel = Some(panel)
    }
    propPanel.get
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

  private def startDrawing(): Unit = Future {
    val image = new WritableImage(width, height)
    val writer = image.pixelWriter
    for (i <- 0 until width par) {
      val cr = rmin + i * (rmax - rmin) / width
      for (j <- 0 until height) {
        val ci = imax - j * (imax - imin) / width
        val cnt = mandelCount(cr, ci)
        writer.setColor(i, j, if (cnt == maxCount) Color.Black else
          Color(1.0, 0.0, 0.0, math.log(cnt.toDouble) / math.log(maxCount)))
      }
    }
    Platform.runLater {
      img = image
      drawing.draw()
    }
  }
}