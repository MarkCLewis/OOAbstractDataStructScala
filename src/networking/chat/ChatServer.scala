package networking.chat

import java.io.InputStream
import java.io.PrintStream
import java.net.ServerSocket
import java.net.Socket
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import io.StdIn.readLine

object ChatServer extends App {
  private case class User(name: String, sock: Socket, is: InputStream, ps: PrintStream)
  private val users = new ConcurrentHashMap[String, User]().asScala

  val port = if (args.isEmpty) 8000 else args(0).toInt
  Future { startServer(port) }
  while (true) {
    for ((name, user) <- users) {
      doChat(user)
    }
    Thread.sleep(100)
  }

  private def startServer(port: Int): Unit = {
    val ss = new ServerSocket(port)
    while (true) {
      val sock = ss.accept()
      addUser(sock)
    }
  }

  private def nonbockingReadLine(is: InputStream): Option[String] = {
    if (is.available() > 0) {
      val buf = Array.fill(is.available())(0.toByte)
      is.read(buf)
      Some(new String(buf).trim)
    } else None
  }

  private def addUser(sock: Socket): Unit = {
    val is = sock.getInputStream()
    val ps = new PrintStream(sock.getOutputStream())
    Future {
      while (is.available() == 0) Thread.sleep(10)
      val name = nonbockingReadLine(is).get
      if (users.contains(name)) {
        ps.println(":quit")
        sock.close()
      } else {
        ps.println(":accept")
        val user = User(name, sock, is, ps)
        users += name -> user
      }
    }
  }

  private def doChat(user: User): Unit = {
    nonbockingReadLine(user.is) match {
      case Some(input) =>
        if (input != ":quit") {
          val index = input.indexOf("<-")
          if (index > 0) {
            val oname = input.take(index).trim
            if (users.contains(oname)) {
              users(oname).ps.println(user.name+" -> "+input.drop(index + 2).trim)
            } else {
              user.ps.println(oname+" is not a valid handle.")
            }
          } else {
            val output = user.name+" : "+input
            for ((name, ouser) <- users; if name != user.name) {
              ouser.ps.println(output)
            }
          }
        } else {
          user.sock.close()
          users.remove(user.name)
        }
      case None =>
    }
  }
}