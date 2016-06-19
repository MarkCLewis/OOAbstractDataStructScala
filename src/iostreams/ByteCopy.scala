package iostreams

import java.io.FileInputStream
import java.io.FileOutputStream

object ByteCopy extends App {
  val fis = new FileInputStream(args(0))
  val fos = new FileOutputStream(args(1))
  var byte = fis.read()
  while (byte >= 0) {
    fos.write(byte)
    byte = fis.read()
  }
  fis.close()
  fos.close()
}