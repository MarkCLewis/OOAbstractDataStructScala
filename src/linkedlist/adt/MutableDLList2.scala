package linkedlist.adt

import collection.mutable

class MutableDLList2[A] extends mutable.Buffer[A] {
  private var numElems = 0
  private trait Node {
    var data: A
    var prev: Node
    var next: Node
  }
  private class DNode(var data: A, var prev: Node, var next: Node) extends Node
  private class ENode extends Node {
    def data: A = throw new IllegalArgumentException("Requested value of sentinel node.")
    def data_=(d: A) = throw new IllegalArgumentException("Value assigned to sentinel node.")
    var next: Node = this
    var prev: Node = this
  }
  private val end = new ENode

  def +=(elem: A) = {
    val newNode = new DNode(elem, end.prev, end)
    end.prev.next = newNode
    end.prev = newNode
    numElems += 1
    this
  }

  def +=:(elem: A) = {
    val newNode = new DNode(elem, end, end.next)
    end.next.prev = newNode
    end.next = newNode
    numElems += 1
    this
  }

  def apply(n: Int): A = {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n+" of "+numElems)
    var rover = end.next
    for (i <- 0 until n) rover = rover.next
    rover.data
  }

  def clear(): Unit = {
    end.prev = end
    end.next = end
    numElems = 0
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    if (n < 0 || n >= numElems + 1) throw new IndexOutOfBoundsException(n+" of "+numElems)
    if (elems.nonEmpty) {
      var rover = end.next
      for (i <- 0 until n) rover = rover.next
      for (e <- elems) {
        val newNode = new DNode(e, rover.prev, rover)
        rover.prev.next = newNode
        rover.prev = newNode
        numElems += 1
      }
    }
  }

  def iterator = new Iterator[A] {
    var rover = end.next
    def hasNext = rover != end
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
    var rover = end.next
    for (i <- 0 until n) rover = rover.next
    val ret = rover.data
    rover.prev.next = rover.next
    rover.next.prev = rover.prev
    ret
  }

  def update(n: Int, newelem: A) {
    if (n < 0 || n >= numElems) throw new IndexOutOfBoundsException(n+" of "+numElems)
    var rover = end.next
    for (i <- 0 until n) rover = rover.next
    rover.data = newelem
  }

  override def toString = mkString("MutableDLList(", ", ", ")")
}
