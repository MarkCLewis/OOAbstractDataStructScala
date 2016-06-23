package networking.chat

import io.StdIn.readLine
import java.io.PrintStream
import java.net.Socket
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ChatClient extends App {
  if (args.isEmpty) {
    println("Usage: scala ChatClient host [port]")
  } else {
    val port = if (args.length > 1) args(1).toInt else 8000
    val sock = new Socket(args(0), port)
    val sc = new Scanner(sock.getInputStream())
    val ps = new PrintStream(sock.getOutputStream())
    println("What is your handle for the chat room?")
    val name = readLine()
    ps.println(name)
    val response = sc.nextLine()
    if (response != ":quit") {
      Future { incoming(sc) }
      println("Welcome! Begin chatting.")
      outgoing(ps)
    } else {
      println("The server has rejected you.")
    }
    sock.close()
  }

  @tailrec private def incoming(sc: Scanner) {
    val line = sc.nextLine()
    println(line)
    incoming(sc)
  }

  @tailrec private def outgoing(ps: PrintStream) {
    print("> ")
    val input = readLine().trim
    ps.println(input)
    if (input != ":quit") outgoing(ps)
  }
}