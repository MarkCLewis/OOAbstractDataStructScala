package linkedlist.adt

class ListStack[A] extends Stack[A] {
  private case class Node(data: A, next: Node)
  private var top: Node = null

  def push(obj: A): Unit = {
    top = Node(obj, top)
  }

  def pop(): A = {
    assert(!isEmpty, "Pop called on an empty stack.")
    val ret = top.data
    top = top.next
    ret
  }

  def peek: A = top.data

  def isEmpty: Boolean = top == null
}