package binaryfiles.adt

import java.io.DataInput
import java.io.DataOutput
import java.io.File
import java.io.RandomAccessFile

import scala.collection.mutable

class FixedRecordList[A](
    file: File,
    reader: DataInput => A,
    writer: (DataOutput, A) => Unit) extends mutable.Buffer[A] {

  private val raf = new RandomAccessFile(file, "rw")
  private var (localHead, localTail, localLen, localFirstFree) = {
    if (raf.length() >= 3 * 8) {
      raf.seek(0)
      val h = raf.readLong()
      val t = raf.readLong()
      val l = raf.readLong()
      val ff = raf.readLong()
      (h, t, l, ff)
    } else {
      raf.seek(0)
      raf.writeLong(-1)
      raf.writeLong(-1)
      raf.writeLong(0)
      raf.writeLong(-1)
      (-1L, -1L, 0L, -1L)
    }
  }

  private def lhead: Long = localHead

  private def lhead_=(h: Long): Unit = {
    raf.seek(0)
    raf.writeLong(h)
    localHead = h
  }

  private def length_=(len: Long): Unit = {
    raf.seek(8 * 2)
    raf.writeLong(len)
    localLen = len
  }

  private def ltail: Long = localTail

  private def ltail_=(t: Long): Unit = {
    raf.seek(8)
    raf.writeLong(t)
    localTail = t
  }

  private def firstFree: Long = localFirstFree

  private def firstFree_=(ff: Long): Unit = {
    raf.seek(3 * 8)
    raf.writeLong(ff)
    localFirstFree = ff
  }

  private def newNodePosition: Long = if (firstFree == -1L) raf.length else {
    val ff = firstFree
    raf.seek(ff)
    firstFree = raf.readLong()
    ff
  }

  def +=(elem: A) = {
    val npos = newNodePosition
    raf.seek(npos)
    raf.writeLong(-1L)
    writer(raf, elem)
    if (lhead == -1L) {
      lhead = npos
    } else {
      raf.seek(ltail)
      raf.writeLong(npos)
    }
    ltail = npos
    length += 1
    this
  }

  def +=:(elem: A) = {
    val npos = newNodePosition
    raf.seek(npos)
    raf.writeLong(lhead)
    writer(raf, elem)
    lhead = npos
    if (ltail == -1L) ltail = npos
    length += 1
    this
  }

  def apply(n: Int): A = {
    if (n >= length) throw new IllegalArgumentException("Requested index "+n+
      " of "+length)
    var i = 0
    var pos = lhead
    while (i <= n) {
      raf.seek(pos)
      pos = raf.readLong()
      i += 1
    }
    reader(raf)
  }

  def clear(): Unit = {
    raf.seek(ltail)
    raf.writeLong(localFirstFree)
    localFirstFree = lhead
    lhead = -1
    ltail = -1
    length = 0
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    if (n > length) throw new IllegalArgumentException("Insert at index "+n+
      " of "+length)
    var i = 0
    var (prev, next) = if (n == 0) (-1L, lhead) else {
      var (pp, nn) = (lhead, -1L)
      while (i < n) {
        raf.seek(pp)
        if (i < n - 1) pp = raf.readLong()
        else nn = raf.readLong()
        i += 1
      }
      (pp, nn)
    }
    if (prev != -1L) raf.seek(prev)
    for (elem <- elems) {
      val npos = newNodePosition
      if (prev == -1L) {
        lhead = npos
        prev = npos
      } else raf.writeLong(npos)
      raf.seek(npos + 8)
      writer(raf, elem)
      raf.seek(npos)
    }
    if (next == -1L) ltail = raf.getFilePointer()
    raf.writeLong(next)
    length += elems.size
  }

  def iterator = new Iterator[A] {
    var pos = lhead
    def hasNext = lhead > -1L
    def next = {
      raf.seek(pos)
      pos = raf.readLong()
      reader(raf)
    }
  }

  def length: Int = localLen.toInt

  def remove(n: Int): A = {
    if (n >= length) throw new IllegalArgumentException("Remove index "+n+
      " of "+length)
    var i = 0
    var pos = lhead
    var last, next = -1L
    while (i <= n) {
      raf.seek(pos)
      if (i == n) {
        next = raf.readLong()
      } else {
        last = pos
        pos = raf.readLong()
      }
      i += 1
    }
    val ret = reader(raf)
    if (last == -1L) {
      lhead = next
    } else {
      raf.seek(last)
      raf.writeLong(next)
    }
    if (pos == ltail) {
      ltail = last
    }
    length -= 1
    ret
  }

  def update(n: Int, elem: A): Unit = {
    if (n >= length) throw new IllegalArgumentException("Updating index "+n+
      " of "+length)
    var i = 0
    var pos = lhead
    while (i <= n) {
      raf.seek(pos)
      pos = raf.readLong()
      i += 1
    }
    writer(raf, elem)
  }

  def close(): Unit = raf.close()
}