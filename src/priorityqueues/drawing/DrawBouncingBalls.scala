package priorityqueues.drawing

import scalafx.Includes._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.layout.VBox
import scalafx.event.ActionEvent
import scalafx.scene.control.Button
import priorityqueues.adt.SortedListPQWithRemove
import priorityqueues.adt.SortedListPQWithRemove

class DrawBouncingBalls(d: Drawing, private var balls: Vector[DrawBouncingBalls.Ball]) extends Drawable(d) {
  @transient private var propPanel: Node = null
  @transient private var workCopy: Array[DrawBouncingBalls.Ball] = null
  @transient private var pq: SortedListPQWithRemove[CollEvent] = null

  override def advance(dt: Double): Unit = {
    if(workCopy == null) {
      workCopy = new Array[DrawBouncingBalls.Ball](balls.length)
      pq = new SortedListPQWithRemove((e1, e2) => e2.time.compareTo(e2.time))
    }
    for (i <- balls.indices) workCopy(i) = balls(i).copy(vy = balls(i).vy + 0.01, time = 0.0)
    for (i <- balls.indices) {
      findEventsFor(i, i + 1 until balls.length, 0.0, dt)
    }
    while (!pq.isEmpty) {
      val event = pq.dequeue()
      event.handle(dt)
    }
    balls = Vector(workCopy.map(_.advanceTo(dt)): _*)
  }

  def draw(gc: GraphicsContext): Unit = {
    gc.fill = Color.Black
    gc.fillRect(0, 0, 400, 400)
    gc.fill = Color.Green
    for (DrawBouncingBalls.Ball(x, y, _, _, s, _) <- balls) {
      gc.fillOval((x - s) * 400, (y - s) * 400, s * 800, s * 800)
    }
  }

  def propertiesPanel(): Node = {
    if (propPanel == null) {
      val panel = new VBox
      propPanel = panel
    }
    propPanel
  }

  override def toString = "Bouncing Balls"

  def toXML: xml.Node =
    <drawable type="bouncingBalls">
      { balls.map(_.toXML) }
    </drawable>

  private def collisionTime(b1: DrawBouncingBalls.Ball, b2: DrawBouncingBalls.Ball): Double = {
    val (sx1, sy1) = (b1.x - b1.vx * b1.time, b1.y - b1.vy * b1.time)
    val (sx2, sy2) = (b2.x - b2.vx * b2.time, b2.y - b2.vy * b2.time)
    val radSum = b1.size + b2.size
    val (dx, dy) = (sx1 - sx2, sy1 - sy2)
    val (dvx, dvy) = (b1.vx - b2.vx, b1.vy - b2.vy)
    val c = dx * dx + dy * dy - radSum * radSum
    val b = 2 * (dx * dvx + dy * dvy)
    val a = dvx * dvx + dvy * dvy
    val root = b * b - 4 * a * c
    if (root < 0) {
      -1.0
    } else {
      (-b - math.sqrt(root)) / (2 * a)
    }
  }

  private def findEventsFor(i: Int, against: Seq[Int], curTime: Double, dt:Double) {
    for (j <- against) {
      val t = collisionTime(workCopy(i), workCopy(j))
      if (t >= curTime && t < dt) {
        pq.enqueue(new BallBallColl(t, i, j))
      }
    }
    for ((tfunc, bfunc) <- DrawBouncingBalls.wallInfo) {
      val t = tfunc(workCopy(i))
      if (t < dt) {  // t >= curTime && 
        pq.enqueue(new BallWallColl(t, i, bfunc))
      }
    }
  }

  private trait CollEvent {
    def time: Double
    def handle(dt: Double): Unit
  }

  private class BallBallColl(val time: Double, val b1: Int, val b2: Int) extends CollEvent {
    def handle(dt: Double) {
      val ball1 = workCopy(b1).advanceTo(time)
      val ball2 = workCopy(b2).advanceTo(time)
      val m1 = ball1.size * ball1.size * ball1.size
      val m2 = ball2.size * ball2.size * ball2.size
      val cmvx = (ball1.vx * m1 + ball2.vx * m2) / (m1 + m2)
      val cmvy = (ball1.vy * m1 + ball2.vy * m2) / (m1 + m2)
      val dx = ball1.x - ball2.x
      val dy = ball1.y - ball2.y
      val dist = math.sqrt(dx * dx + dy * dy)
      if (dist > 1.01 * (ball1.size + ball2.size)) {
        println("Warning: collision with big separation. "+b1+" "+b2+" "+dist)
      }
      if (dist < 0.99 * (ball1.size + ball2.size)) {
        println("Warning: collision with little separation. "+b1+" "+b2+" "+dist)
      }
      val vx1 = ball1.vx - cmvx
      val vy1 = ball1.vy - cmvy
      val nx = dx / dist
      val ny = dy / dist
      val mag = nx * vx1 + ny * vy1
      workCopy(b1) = ball1.copy(vx = ball1.vx - 2.0 * mag * nx, vy = ball1.vy - 2.0 * mag * ny)
      workCopy(b2) = ball2.copy(vx = ball2.vx + 2.0 * mag * nx * m1 / m2, vy = ball2.vy + 2.0 * mag * ny * m1 / m2)
      pq.removeMatches(_ match {
        case bbc: BallBallColl => bbc.b1 == b1 || bbc.b2 == b1 || bbc.b1 == b2 || bbc.b2 == b2
        case bwc: BallWallColl => bwc.b == b1 || bwc.b == b2
        case _ => false
      })
      val others = workCopy.indices.filter(b => b != b1 && b != b2)
      findEventsFor(b1, others, time, dt)
      findEventsFor(b2, others, time, dt)
    }
  }

  private class BallWallColl(val time: Double, val b: Int, val newDir: (Double, Double, Double, Double) => (Double, Double)) extends CollEvent {
    def handle(dt: Double) {
      val ball = workCopy(b)
      val nx = ball.x + (time - ball.time) * ball.vx
      val ny = ball.y + (time - ball.time) * ball.vy
      val (nvx, nvy) = newDir(ball.x, ball.y, ball.vx, ball.vy)
      workCopy(b) = DrawBouncingBalls.Ball(nx, ny, nvx, nvy, ball.size, time)
      pq.removeMatches(_ match {
        case bbc: BallBallColl => bbc.b1 == b || bbc.b2 == b
        case bwc: BallWallColl => bwc.b == b
        case _ => false
      })
      findEventsFor(b, workCopy.indices.filter(_ != b), time, dt)
    }
  }
}

object DrawBouncingBalls {
  def apply(d: Drawing, minSize: Double = 0.01, maxSize: Double = 0.05) = {
    new DrawBouncingBalls(d, Vector.fill(20) {
      val size = minSize + math.random * (maxSize - minSize)
      Ball(size + math.random * (1 - 2 * size), size + math.random * (1 - 2 * size),
        (math.random - 0.5) * 0.02, (math.random - 0.5) * 0.02, size, 0.0)
    })
  }

  def apply(data: xml.Node, d: Drawing) = {
    new DrawBouncingBalls(d, Vector((data \ "ball").map(bxml => {
      val x = (bxml \ "@x").text.toDouble
      val y = (bxml \ "@y").text.toDouble
      val vx = (bxml \ "@vx").text.toDouble
      val vy = (bxml \ "@vy").text.toDouble
      val size = (bxml \ "@size").text.toDouble
      Ball(x, y, vx, vy, size, 0.0)
    }): _*))
  }

  case class Ball(x: Double, y: Double, vx: Double, vy: Double, size: Double, time: Double) {
    def toXML = <ball x={ x.toString } y={ y.toString } vx={ vx.toString } vy={ vy.toString } size={ size.toString }/>

    def advanceTo(t: Double) = {
      val dt = t - time
      copy(x = x + vx * dt, y = y + vy * dt, time = t)
    }
  }

  private val wallInfo = Seq[(Ball => Double, (Double, Double, Double, Double) => (Double, Double))](
    (b => if (b.vx < 0) b.time - (b.x - b.size) / b.vx else 1000, (x, y, vx, vy) => (vx.abs, vy)),
    (b => if (b.vx > 0) b.time + (1 - b.x - b.size) / b.vx else 1000, (x, y, vx, vy) => (-vx.abs, vy)),
    (b => if (b.vy < 0) b.time - (b.y - b.size) / b.vy else 1000, (x, y, vx, vy) => (vx, vy.abs)),
    (b => if (b.vy > 0) b.time + (1 - b.y - b.size) / b.vy else 1000, (x, y, vx, vy) => (vx, -vy.abs)))
}