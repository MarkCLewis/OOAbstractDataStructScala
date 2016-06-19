package iostreams

import java.io.IOException
import java.io.FileInputStream
import java.io.FileNotFoundException

object LoanPattern extends App {
  def useFileInputStream[A](fileName: String)(body: FileInputStream => A): A = {
    val fis = new FileInputStream(fileName)
    try {
      body(fis)
    } finally {
      fis.close()
    }
  }

  try {
    useFileInputStream(args(0))(fis => {
      var byte = fis.read()
      while (byte >= 0) {
        print(byte+" ")
        byte = fis.read()
      }
      println()
    })
  } catch {
    case ex: FileNotFoundException =>
      println("The file "+args(0)+" could not be opened.")
    case ex: IOException =>
      println("There was a problem working with the file.")
  }
}