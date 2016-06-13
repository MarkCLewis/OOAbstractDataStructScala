package stackqueue.adt

import scala.reflect.ClassTag

class ArrayQueue[A: ClassTag] extends Queue[A] {
  private var front, back = 0
  private var data = new Array[A](10)

  def enqueue(obj: A) {
    if ((back + 1) % data.length == front) {
      val tmp = new Array[A](data.length * 2)
      // Array.copy(data, 0, tmp, 0, data.length)
      for (i <- 0 until data.length - 1) tmp(i) = data((i + front) % data.length)
      front = 0
      back = data.length - 1
      data = tmp
    }
    data(back) = obj
    back = (back + 1) % data.length
  }

  def dequeue(): A = {
    assert(!isEmpty, "Dequeue called on an empty queue.")
    val ret = data(front)
    front = (front + 1) % data.length
    ret
  }

  def peek: A = data(front)

  def isEmpty: Boolean = front == back
}