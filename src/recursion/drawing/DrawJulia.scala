package recursion.drawing

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

class DrawJulia(
    d: Drawing,
    private var rmin: Double,
    private var rmax: Double,
    private var imin: Double,
    private var imax: Double,
    private var creal: Double,
    private var cimag: Double,
    private var width: Int,
    private var height: Int,
    private var maxCount: Int) extends Drawable(d) {
  @transient private var img = new WritableImage(width, height)
  @transient private var juliaActor: ActorRef = null
  @transient private var router: ActorRef = null
  @transient private var propPanel: Node = null

  makeActors()
  juliaActor ! MakeImage(drawing.graphicsContext)

  override def toString() = "Julia Set"

  def draw(gc: GraphicsContext): Unit = {
    if (img != null) gc.drawImage(img, 0, 0)
    else {
      if (juliaActor == null) makeActors()
      juliaActor ! MakeImage(gc)
    }
  }

  def propertiesPanel(): Node = {
    def checkChangeMade[A](originalValue: A, newValue: => A): A = {
      try {
        val nv = newValue
        if (originalValue == nv) originalValue else {
          if (juliaActor == null) makeActors()
          juliaActor ! MakeImage(drawing.graphicsContext)
          nv
        }
      } catch {
        case nfe: NumberFormatException => originalValue
      }
    }
    if (propPanel == null) {
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
      propPanel = panel
    }
    propPanel
  }

  def toXML: xml.Node = {
    <drawable type="julia" rmin={ rmin.toString() } rmax={ rmax.toString() } imin={ imin.toString() } imax={ imax.toString() } creal={ creal.toString() } cimag={ cimag.toString() } width={ width.toString() } height={ height.toString() } maxCount={ maxCount.toString() }/>
  }

  private def makeActors(): Unit = {
    juliaActor = DrawingMain.system.actorOf(Props(new JuliaActor))
    router = DrawingMain.system.actorOf(BalancingPool(5).props(Props(new LineActor)), "poolRouter")
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

  case class MakeImage(gc:GraphicsContext)
  case class Line(row: Int, imaginary: Double, gc:GraphicsContext)
  case class LineResult(row: Int, colors: Array[Color], gc:GraphicsContext)

  class JuliaActor extends Actor {
    private var image: WritableImage = null
    private var writer: PixelWriter = null
    private var lineCount = 0
    def receive = {
      case MakeImage(gc) =>
        image = new WritableImage(width, height)
        writer = image.pixelWriter
        lineCount = 0
        for (i <- 0 until height) {
          val imag = imax - i * (imax - imin) / width
          router ! Line(i, imag, gc)
        }
      case LineResult(r, colors, gc) =>
        lineCount += 1
        for (i <- colors.indices) {
          writer.setColor(i, r, colors(i))
        }
        if (lineCount >= height) {
          Platform.runLater {
            img = image
            drawing.drawTo(gc)
          }
        }
    }
  }

  class LineActor extends Actor {
    def receive = {
      case Line(r, imag, gc) =>
        sender ! LineResult(r, Array.tabulate(width)(i => {
          val real = rmin + i * (rmax - rmin) / width
          val cnt = juliaCount(real, imag)
          if (cnt == maxCount) Color.Black else
            Color(1.0, 0.0, 0.0, math.log(cnt.toDouble) / math.log(maxCount))
        }), gc)
    }
  }
}

object DrawJulia {
  def apply(d: Drawing) = new DrawJulia(d, -1, 1, -1, 1, -0.5, 0.6, 600, 600, 100)

  def apply(n: xml.Node, d: Drawing) = {
    val rmin = (n \ "@rmin").text.toDouble
    val rmax = (n \ "@rmax").text.toDouble
    val imin = (n \ "@imin").text.toDouble
    val imax = (n \ "@imax").text.toDouble
    val creal = (n \ "@creal").text.toDouble
    val cimag = (n \ "@cimag").text.toDouble
    val width = (n \ "@width").text.toInt
    val height = (n \ "@height").text.toInt
    val maxCount = (n \ "@maxCount").text.toInt
    new DrawJulia(d, rmin, rmax, imin, imax, creal, cimag, width, height, maxCount)
  }
}