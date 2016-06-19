package iostreams

import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException

object ReadBytesTryFinally extends App {
  try {
    val fis = new FileInputStream(args(0))
    try {
      var byte = fis.read()
      while (byte >= 0) {
        print(byte+" ")
        byte = fis.read()
      }
      println()
    } catch {
      case ex: IOException =>
        println("There was a problem working with the file.")
    } finally {
      fis.close()
    }
  } catch {
    case ex: FileNotFoundException =>
      println("The file "+args(0)+" could not be opened.")
  }
}