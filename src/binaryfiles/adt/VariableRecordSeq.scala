package binaryfiles.adt

import collection.mutable
import java.io._

class VariableRecordSeq[A](
    index: File,
    data: File,
    reader: DataInput => A,
    writer: (DataOutput, A) => Unit) extends mutable.IndexedSeq[A] {

  private val indexFile = new RandomAccessFile(index, "rw")
  private val dataFile = new RandomAccessFile(data, "rw")

  def apply(index: Int): A = {
    indexFile.seek(12 * index)
    val pos = indexFile.readLong()
    dataFile.seek(pos)
    reader(dataFile)
  }

  def update(index: Int, a: A): Unit = {
    val baos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(baos)
    writer(dos, a)
    val outData = baos.toByteArray()
    val (pos, len) = if (index < length) {
      indexFile.seek(12 * index)
      val p = indexFile.readLong()
      val l = indexFile.readInt()
      if (baos.size() <= l) (p, l) else (dataFile.length(), outData.length)
    } else (dataFile.length(), outData.length)
    dataFile.seek(pos)
    dataFile.write(outData)
    indexFile.seek(12 * index)
    indexFile.writeLong(pos)
    indexFile.writeInt(len)
  }

  def length: Int = (indexFile.length() / 12).toInt

  def close(): Unit = {
    indexFile.close()
    dataFile.close()
  }
}