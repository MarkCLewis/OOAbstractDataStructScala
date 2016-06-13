package guigraphics

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.BorderPane
import scalafx.animation.AnimationTimer
import oodetails.Vect2D
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyEvent

object AnimationExample extends JFXApp {
  case class Swarmer(p: Vect2D, speed: Double)
  private var swarm = List.tabulate(20)(i => Swarmer(Vect2D(300, 300), (i * 5 + 20) * 0.02))
  private var mouse = Vect2D(300, 300)
  private var box = Vect2D(0, 0)
  private var upPressed = false
  private var downPressed = false
  private var leftPressed = false
  private var rightPressed = false
  private val speed = 25.0
  stage = new JFXApp.PrimaryStage {
    title = "Animation Example"
    scene = new Scene(600, 600) {
      val border = new BorderPane
      val canvas = new Canvas(600, 600)
      val gc = canvas.graphicsContext2D
      border.center = canvas
      root = border

      canvas.width <== border.width
      canvas.height <== border.height

      canvas.onMouseMoved = (me: MouseEvent) => {
        mouse = Vect2D(me.x, me.y)
      }

      canvas.onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => upPressed = true
          case KeyCode.Down => downPressed = true
          case KeyCode.Left => leftPressed = true
          case KeyCode.Right => rightPressed = true
          case _ =>
        }
      }
      canvas.onKeyReleased = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => upPressed = false
          case KeyCode.Down => downPressed = false
          case KeyCode.Left => leftPressed = false
          case KeyCode.Right => rightPressed = false
          case _ =>
        }
      }

      var lastTime = 0L
      val timer = AnimationTimer(time => {
        if (lastTime == 0) lastTime = time
        else {
          val interval = (time - lastTime) / 1e9
          lastTime = time
          swarm = swarm.map(s => s.copy(p = s.p + (mouse - s.p) * (interval * s.speed)))
          if (upPressed) box += Vect2D(0, -interval * speed)
          if (downPressed) box += Vect2D(0, interval * speed)
          if (leftPressed) box += Vect2D(-interval * speed, 0)
          if (rightPressed) box += Vect2D(interval * speed, 0)
        }
        gc.fill = Color.White
        gc.fillRect(0, 0, canvas.width.value, canvas.height.value)
        gc.fill = Color.Red
        for (Swarmer(p, _) <- swarm) {
          gc.fillOval(p.x, p.y, 20, 20)
        }
        gc.fill = Color.Green
        gc.fillRect(box.x, box.y, 20, 20)
      })
      timer.start
      canvas.requestFocus()
    }
  }

}