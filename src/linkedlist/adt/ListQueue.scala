package linkedlist.adt

class ListQueue[A] extends Queue[A] {
  private class Node(val data: A, var next: Node)
  private var front: Node = null
  private var back: Node = null

  def enqueue(obj: A): Unit = {
    if (front == null) {
      front = new Node(obj, null)
      back = front
    } else {
      back.next = new Node(obj, null)
      back = back.next
    }
  }

  def dequeue(): A = {
    assert(!isEmpty, "Dequeue called on an empty queue.")
    val ret = front.data
    front = front.next
    if (front == null) back = null
    ret
  }

  def peek: A = front.data

  def isEmpty: Boolean = front == null
}