package guigraphics

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.image.ImageView
import scalafx.scene.shape.Rectangle
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode

object KeyboardMouseInput extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "Keyboard and Mouse Input"
    scene = new Scene(600, 600) {
      val img = new ImageView("file:Mimas_Cassini.jpg")
      val box = Rectangle(0, 0, 30, 30)
      box.fill = Color.Green
      content = List(img, box)
      onMouseMoved = (me: MouseEvent) => {
        img.x = me.x
        img.y = me.y
      }
      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Up => box.y = box.y.value - 2
          case KeyCode.Down => box.y = box.y.value + 2
          case KeyCode.Left => box.x = box.x.value - 2
          case KeyCode.Right => box.x = box.x.value + 2
          case _ =>
        }
      }
    }
  }
}