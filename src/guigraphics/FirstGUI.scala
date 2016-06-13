package guigraphics

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.shape.Ellipse
import scalafx.scene.paint.Color

object FirstGUI extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "First GUI"
    scene = new Scene(400,200) {
      val button = new Button("Click Me")
      button.layoutX = 50
      button.layoutY = 50
      val ellipse = Ellipse(300, 50, 50, 25)
      ellipse.fill = Color.Red
      content = List(button, ellipse)
    }
  }
}