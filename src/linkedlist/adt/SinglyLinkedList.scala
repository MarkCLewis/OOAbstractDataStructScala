package linkedlist.adt

/**
 * This is a very simple implementation of a basic linked list.
 * @tparam A the type stored in the list.
 */
class SinglyLinkedList[A] {
  private class Node(var data: A, var next: Node)
  private var head: Node = null

  /**
   * This methods gets the value at a specified index. If the index is beyond the
   * length of the list a NullPointerException will be thrown.
   * @param index the index to get.
   * @return the value at that index.
   */
  def apply(index: Int): A = {
    assert(index >= 0)
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data
  }

  /**
   * Sets the value at a particular index in the list. If the index is beyond the
   * length of the list a NullPointerException will be thrown.
   * @param index the index to set the value at.
   * @param data the value to store at that index.
   */
  def update(index: Int, data: A): Unit = {
    assert(index >= 0)
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data = data
  }

  /**
   * Inserts a new value at a particular index in the list. If the index is beyond
   * the length of the list a NullPointerException will be thrown.
   * @param index the index to set the value at.
   * @param data the value to insert at that index.
   */
  def insert(index: Int, data: A): Unit = {
    assert(index >= 0)
    if (index == 0) {
      head = new Node(data, head)
    } else {
      var rover = head
      for (i <- 0 until index - 1) rover = rover.next
      rover.next = new Node(data, rover.next)
    }
  }

  /**
   * Removes a particular index from the list. If the index is beyond the length
   * of the list a NullPointerException will be thrown.
   * @param index the index to remove.
   * @return the data value that had been stored at that index.
   */
  def remove(index: Int): A = {
    assert(index >= 0)
    if (index == 0) {
      val ret = head.data
      head = head.next
      ret
    } else {
      var rover = head
      for (i <- 0 until index - 1) rover = rover.next
      val ret = rover.next.data
      rover.next = rover.next.next
      ret
    }
  }
}