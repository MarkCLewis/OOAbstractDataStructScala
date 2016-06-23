package networking

import java.net.ServerSocket

object DoServer extends App {
  val ss = new ServerSocket(8000)
  val serverSock = ss.accept()
  // Stuff to communicate across serverSock
}