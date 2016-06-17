package multithreading1

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.event.ActionEvent

object SimpleThreadGUI extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "GUI Thread Demo"
    scene = new Scene(100, 50) {
      val button = new Button("Click Me")
      root = button
      button.onAction = (ae: ActionEvent) => println(Thread.currentThread().getName)
    }
  }
}