package regexparser

object CodeSegments extends App {
  val fileName = "lines.txt"

  val NumberedLine = """\s*(\d+)\.(.+)""".r
  val source = io.Source.fromFile(fileName)
  val lines = source.getLines
  val numberedLines = (for (NumberedLine(num, text) <- lines) yield {
    num -> text
  }).toMap
  source.close
}