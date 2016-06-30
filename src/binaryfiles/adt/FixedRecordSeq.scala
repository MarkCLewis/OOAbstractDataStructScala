package binaryfiles.adt

import java.io.DataInput
import java.io.DataOutput
import java.io.File
import java.io.RandomAccessFile

import scala.collection.mutable

class FixedRecordSeq[A](
    file: File,
    recordLength: Int,
    reader: DataInput => A,
    writer: (DataOutput, A) => Unit) extends mutable.IndexedSeq[A] {

  private val raf = new RandomAccessFile(file, "rw")

  def apply(index: Int): A = {
    raf.seek(recordLength * index)
    reader(raf)
  }

  def update(index: Int, a: A): Unit = {
    raf.seek(recordLength * index)
    writer(raf, a)
  }

  def length: Int = (raf.length() / recordLength).toInt

  def close(): Unit = raf.close()
}