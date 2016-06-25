package networking.drawing

import stackqueue.util.RPNCalc
import scalafx.application.Platform

object Commands {
  private val commands = Map[String, (String, Drawing) => Any](
    "add" -> ((rest, d) => rest.trim.split(" +").map(_.toInt).sum),
    "echo" -> ((rest, d) => rest.trim),
    "refresh" -> ((rest, d) => Platform.runLater { d.draw() } ),
    "rpn" -> ((rest,d) => (RPNCalc(rest.trim.split(" +"), d.vars))),
    "set" -> ((rest,d) => {
      val parts = rest.trim.split(" +")
      d.vars(parts(0)) = parts(1).toDouble
      parts(0)+" = "+parts(1)
    }),
    "freeze" -> ((rest,d) => Thread.sleep(rest.trim.toInt*1000)))

  def apply(input: String, drawing: Drawing): Any = {
    val spaceIndex = input.indexOf(' ')
    val (command, rest) = if (spaceIndex < 0) (input.toLowerCase(), "") 
        else (input.take(spaceIndex).toLowerCase(), input.drop(spaceIndex))
    if (commands.contains(command)) commands(command)(rest, drawing) else "Not a valid command."
  }
}