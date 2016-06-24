package linkedlist.adt

/**
 * This trait defines a mutable Stack ADT.
 * @tparam A the type of data stored 
 */
trait Stack[A] {
  /**
   * Add an item to the stack.
   * @param obj the item to add
   */
  def push(obj:A)
  
  /**
   * Remove the most recently added item.
   * @return the item that was removed
   */
  def pop():A
  
  /**
   * Return the most recently added item without removing it.
   * @return the most recently added item
   */
  def peek:A
  
  /**
   * Tells whether this stack is empty.
   * @return true if there are no items on the stack, otherwise false.
   */
  def isEmpty:Boolean
}