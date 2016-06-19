package iostreams

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream

object ReadWriteBinary {
  def withDOS[A](fileName: String)(body: DataOutputStream => A): A = {
    val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))
    try {
      body(dos)
    } finally {
      dos.close()
    }
  }

  def withDIS[A](fileName: String)(body: DataInputStream => A): A = {
    val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(fileName)))
    try {
      body(dis)
    } finally {
      dis.close()
    }
  }

  def writeDoubleArray(fileName: String, data: Array[Double]): Unit = {
    withDOS(fileName)(dos => {
      dos.writeInt(data.size)
      data.foreach(x => dos.writeDouble(x))
    })
  }

  def readDoubleArray(fileName: String): Array[Double] = {
    withDIS(fileName)(dis => {
      Array.fill(dis.readInt)(dis.readDouble)
    })
  }
}