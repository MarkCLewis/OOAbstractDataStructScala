package iostreams.drawing

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.image.WritableImage
import scalafx.scene.image.PixelWriter
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.application.Platform
import akka.actor.Props
import akka.routing.BalancingPool
import akka.actor.Actor
import akka.actor.ActorRef

class DrawJulia(d: Drawing) extends Drawable(d) {
  private var (rmin, rmax, imin, imax) = (-1.0, 1.0, -1.0, 1.0)
  private var (creal, cimag) = (-0.5, 0.6)
  private var (width, height) = (600, 600)
  private var maxCount = 100
  private var img = new WritableImage(width, height)
  private val juliaActor = DrawingMain.system.actorOf(Props(new JuliaActor), "JuliaActor")
  private val router = DrawingMain.system.actorOf(BalancingPool(5).props(Props(new LineActor)), "poolRouter")
  private var propPanel: Option[Node] = None

  juliaActor ! MakeImage

  override def toString() = "Julia Set"

  def draw(gc: GraphicsContext): Unit = {
    gc.drawImage(img, 0, 0)
  }

  def propertiesPanel(): Node = {
    def checkChangeMade[A](originalValue: A, newValue: => A): A = {
      try {
        val nv = newValue
        if (originalValue == nv) originalValue else {
          juliaActor ! MakeImage
          nv
        }
      } catch {
        case nfe: NumberFormatException => originalValue
      }
    }
    if (propPanel.isEmpty) {
      val panel = new VBox
      panel.children = List(
        DrawingMain.labeledTextField("C-real", creal.toString(), s => { creal = checkChangeMade(creal, s.toDouble) }),
        DrawingMain.labeledTextField("C-imaginary", cimag.toString(), s => { cimag = checkChangeMade(cimag, s.toDouble) }),
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

  private def juliaIter(zr: Double, zi: Double, cr: Double, ci: Double) = (zr * zr - zi * zi + cr, 2 * zr * zi + ci)

  private def juliaCount(z1r: Double, z1i: Double): Int = {
    var ret = 0
    var (zr, zi) = (z1r, z1i)
    while (ret < maxCount && zr * zr + zi * zi < 4) {
      val (tr, ti) = juliaIter(zr, zi, creal, cimag)
      zr = tr
      zi = ti
      ret += 1
    }
    ret
  }

  case object MakeImage
  case class Line(row: Int, imaginary: Double)
  case class LineResult(row: Int, colors: Array[Color])

  class JuliaActor extends Actor {
    private var image: WritableImage = null
    private var writer: PixelWriter = null
    private var lineCount = 0
    def receive = {
      case MakeImage =>
        image = new WritableImage(width, height)
        writer = image.pixelWriter
        lineCount = 0
        for (i <- 0 until height) {
          val imag = imax - i * (imax - imin) / width
          router ! Line(i, imag)
        }
      case LineResult(r, colors) =>
        lineCount += 1
        for (i <- colors.indices) {
          writer.setColor(i, r, colors(i))
        }
        if (lineCount >= height) {
          Platform.runLater {
            img = image
            drawing.draw()
          }
        }
    }
  }

  class LineActor extends Actor {
    def receive = {
      case Line(r, imag) =>
        sender ! LineResult(r, Array.tabulate(width)(i => {
          val real = rmin + i * (rmax - rmin) / width
          val cnt = juliaCount(real, imag)
          if (cnt == maxCount) Color.Black else
            Color(1.0, 0.0, 0.0, math.log(cnt.toDouble) / math.log(maxCount))
        }))
    }
  }
}