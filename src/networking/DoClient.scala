package networking

import java.net.Socket

object DoClient extends App {
  val clientSock = new Socket("localhost", 8000)
  // Stuff to communicate across clientSock
}