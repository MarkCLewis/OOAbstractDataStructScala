package mapset.drawing

object Commands {
  private val commands = Map[String, (String, Drawing) => Any](
    "add" -> ((rest, d) => rest.trim.split(" +").map(_.toInt).sum),
    "echo" -> ((rest, d) => rest.trim),
    "refresh" -> ((rest, d) => d.draw()))

  def apply(input: String, drawing: Drawing): Any = {
    val spaceIndex = input.indexOf(' ')
    val (command, rest) = if (spaceIndex < 0) (input.toLowerCase(), "") 
        else (input.take(spaceIndex).toLowerCase(), input.drop(spaceIndex))
    if (commands.contains(command)) commands(command)(rest, drawing) else "Not a valid command."
  }
}