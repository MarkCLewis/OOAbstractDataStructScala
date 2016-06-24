package linkedlist.adt

import collection.mutable

class MutableSLList[A] extends mutable.Buffer[A] {
  private var hd: Node = null
  private var tl: Node = null
  private var numElems = 0
  private class Node(var data: A, var next: Node)

  def +=(elem: A) = {
    if (tl == null) {
      tl = new Node(elem, null)
      hd = tl
    } else {
      tl.next = new Node(elem, null)
      tl = tl.next
    }
    numElems += 1
    this
  }

  def +=:(elem: A) = {
    hd = new Node(elem, hd)
    if (tl == null) {
      tl = hd
    }
    numElems += 1
    this
  }

  def apply(n: Int): A = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n+" of "+numElems)
    var rover = hd
    for (i <- 0 until n) rover = rover.next
    rover.data
  }

  def clear(): Unit = {
    hd = null
    tl = null
    numElems = 0
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    if (n < 0 || n >= numElems + 1) throw new IndexOutOfBoundsException(n+" of "+numElems)
    if (elems.nonEmpty) {
      val first = new Node(elems.head, null)
      var last = first
      numElems += 1
      for (e <- elems.tail) {
        last.next = new Node(e, null)
        last = last.next
        numElems += 1
      }
      if (n == 0) {
        last.next = hd
        hd = first
        if (tl == null) tl = last
      } else {
        var rover = hd
        for (i <- 1 until n) rover = rover.next
        last.next = rover.next
        rover.next = first
        if (last.next == null) tl = last
      }
    }
  }

  def iterator = new Iterator[A] {
    var rover = hd
    def hasNext = rover != null
    def next: A = {
      val ret = rover.data
      rover = rover.next
      ret
    }
  }

  def length: Int = numElems

  def remove(n: Int): A = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n+" of "+numElems)
    numElems -= 1
    if (n == 0) {
      val ret = hd.data
      hd = hd.next
      if (hd == null) tl = null
      ret
    } else {
      var rover = hd
      for (i <- 1 until n) rover = rover.next
      val ret = rover.next.data
      rover.next = rover.next.next
      if (rover.next == null) tl = rover
      ret
    }
  }

  def update(n: Int, newelem: A): Unit = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n+" of "+numElems)
    var rover = hd
    for (i <- 0 until n) rover = rover.next
    rover.data = newelem
  }

  override def toString = mkString("MutableSLList(", ", ", ")")
}