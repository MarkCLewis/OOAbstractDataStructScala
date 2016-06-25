package priorityqueues.adt

/**
 * This trait defines a mutable Priority Queue ADT.
 * @tparam A the type of data stored 
 */
trait PriorityQueue[A] {
  /**
   * Add an item to the priority queue.
   * @param obj the item to add
   */
  def enqueue(obj: A): Unit
  
  /**
   * Remove the item that has the highest priority or, in the case of a tie, 
   * has been on the longest.
   * @return the item that was removed
   */
  def dequeue(): A
  
  /**
   * Return the item that has the highest priority or, in the case of a tie, 
   * has been on the longest without removing it.
   * @return the most recently added item
   */
  def peek: A
  
  /**
   * Tells whether this priority queue is empty.
   * @return true if there are no items on the priority queue, otherwise false.
   */
  def isEmpty: Boolean
}
