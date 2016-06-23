package networking

import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetAddress
import scala.collection.mutable

object Datagram extends App {
  def packDouble(d: Double, v: mutable.IndexedSeq[Byte]):Unit = {
    var dlong = java.lang.Double.doubleToLongBits(d)
    for (i <- v.indices) {
      v(i) = dlong.toByte
      dlong >>= 8
    }
  }

  def unpackDouble(v: mutable.IndexedSeq[Byte]): Double = {
    val dlong = v.foldRight(0L)((b, dl) => (dl << 8) | (b & 0xff))
    java.lang.Double.longBitsToDouble(dlong)
  }

  def packArray(ds: Array[Double]): Array[Byte] = {
    val ret = new Array[Byte](8 * ds.length)
    for (i <- ds.indices) packDouble(ds(i), ret.view(8 * i, 8 * (i + 1)))
    ret
  }

  if (args.length > 1) {
    val socket = new DatagramSocket()
    val data = packArray(args.map(_.toDouble))
    val packet = new DatagramPacket(data, data.length, InetAddress.getByName("localhost"), 8000)
    socket.send(packet)
  } else if (args.length == 1) {
    val socket = new DatagramSocket(8000)
    val num = args(0).toInt
    val data = new Array[Byte](8 * num)
    val packet = new DatagramPacket(data, data.length)
    socket.receive(packet)
    for (i <- 0 until 8 * num by 8) println(unpackDouble(data.view(i, i + 8)))
  } else println("Specify one int for number to read or multiple doubles to send.")
}