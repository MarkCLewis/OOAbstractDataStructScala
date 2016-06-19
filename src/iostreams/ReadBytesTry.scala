package iostreams

import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException

object ReadBytesTry extends App {
  try {
    val fis = new FileInputStream(args(0))
    var byte = fis.read()
    while (byte >= 0) {
      print(byte+" ")
      byte = fis.read()
    }
    println()
    fis.close()
  } catch {
    case ex: FileNotFoundException =>
      println("The file "+args(0)+" could not be opened.")
    case ex: IOException =>
      println("There was a problem working with the file.")
  }
}