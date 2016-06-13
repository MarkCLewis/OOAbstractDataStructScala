package stackqueue.adt

/**
 * This trait defines a mutable Queue ADT.
 * @tparam A the type of data stored
 */
trait Queue[A] {
  /**
   * Add an item to the queue.
   * @param obj the item to add
   */
  def enqueue(obj: A)

  /**
   * Remove the item that has been on the longest.
   * @return the item that was removed
   */
  def dequeue(): A

  /**
   * Return the item that has been on the longest without removing it.
   * @return the most recently added item
   */
  def peek: A

  /**
   * Tells whether this queue is empty.
   * @return true if there are no items on the queue, otherwise false.
   */
  def isEmpty: Boolean
}