package iostreams

import java.io.FileInputStream

object ReadBytes extends App {
  val fis = new FileInputStream(args(0))
  var byte = fis.read()
  while (byte >= 0) {
    print(byte+" ")
    byte = fis.read()
  }
  println()
  fis.close()
}