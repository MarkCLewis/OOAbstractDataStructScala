package regexparser.drawing

import stackqueue.util.RPNCalc
import scalafx.application.Platform
import regexparser.util.Formula

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
    "freeze" -> ((rest,d) => Thread.sleep(rest.trim.toInt*1000)),
    "eval" -> ((rest, d) => Formula.eval(rest, d.vars)))

  val commandSplit = """\s*(\w+)(\s+(.*))?\s*""".r
  
  def apply(input:String, drawing:Drawing):Any = {
    val commandSplit(command, _, rest) = input 
    if (commands.contains(command)) commands(command)(rest, drawing)
      else "Not a valid command."
  }
}