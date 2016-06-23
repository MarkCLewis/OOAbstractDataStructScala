package networking

import java.net.URL
import java.io.BufferedInputStream
import scala.collection.mutable

object RandomNetworkingCode extends App {
  val scalaURL = new URL("http://www.scala-lang.org/")

  {
    val urlis = new BufferedInputStream(scalaURL.openStream())
    val buffer = mutable.Buffer[Byte]()
    var res = urlis.read()
    while (res >= 0) {
      buffer += res.toByte
      res = urlis.read()
    }
    val contents = new String(buffer.toArray)
  }

  {
    val connection = scalaURL.openConnection()
    val urlis = new BufferedInputStream(connection.getInputStream())
    val buffer = new Array[Byte](connection.getContentLength)
    urlis.read(buffer)
    val contents = new String(buffer)
  }
  
  {
    val url = new URL("http://www.cs.trinity.edu/~mlewis/banish.gif")
    url.getContent match {
      case img:java.awt.Image => println("Image")
      case c => println("Not image "+c)
    }
  }
}