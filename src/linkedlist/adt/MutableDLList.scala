package linkedlist.adt

import collection.mutable
import scala.collection.generic.CanBuildFrom

class MutableDLList[A] extends mutable.Buffer[A] {
  private var default: A = _
  private var numElems = 0
  private class Node(var data: A, var prev: Node, var next: Node)
  private val end = new Node(default, null, null)
  end.next = end
  end.prev = end

  def +=(elem: A) = {
    val newNode = new Node(elem, end.prev, end)
    end.prev.next = newNode
    end.prev = newNode
    numElems += 1
    this
  }

  def +=:(elem: A) = {
    val newNode = new Node(elem, end, end.next)
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
        val newNode = new Node(e, rover.prev, rover)
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
  
  override def foreach[U](f: A => U): Unit = {
    var rover = end.next
    while(rover != end) {
      f(rover.data)
      rover = rover.next
    }
  }
  
  override def filter(pred: A => Boolean): MutableDLList[A] = {
    val ret = new MutableDLList[A]()
    var rover = end.next
    while(rover != end) {
      if (pred(rover.data)) ret += rover.data
      rover = rover.next
    }
    ret
  }
  
  def myMap[B](f: A => B): MutableDLList[B] = {
    val ret = new MutableDLList[B]()
    var rover = end.next
    while(rover != end) {
      ret += f(rover.data)
      rover = rover.next
    }
    ret
  }

  override def toString = mkString("MutableDLList(", ", ", ")")
}
