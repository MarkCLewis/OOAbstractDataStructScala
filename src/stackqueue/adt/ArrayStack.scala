package stackqueue.adt

import scala.reflect.ClassTag

class ArrayStack[A: ClassTag] extends Stack[A] {
  private var top = 0
  private var data = new Array[A](10)

  def push(obj: A) {
    if (top >= data.length) {
      val tmp = new Array[A](data.length * 2)
      for (i <- 0 until data.length) tmp(i) = data(i)
      data = tmp
    }
    data(top) = obj
    top += 1
  }

  def pop(): A = {
    assert(!isEmpty, "Pop called on an empty stack.")
    top -= 1
    data(top)
  }

  def peek: A = data(top - 1)

  def isEmpty: Boolean = top == 0
}