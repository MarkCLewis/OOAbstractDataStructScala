package linkedlist.adt

import collection.immutable.LinearSeq

sealed abstract class ImmutableSLList[+A] extends LinearSeq[A] {
  def ::[B >: A](elem: B): ImmutableSLList[B] = new Cons(elem, this)

  override def iterator = new Iterator[A] {
    var rover: LinearSeq[A] = ImmutableSLList.this
    def hasNext = !rover.isEmpty
    def next: A = {
      val ret = rover.head
      rover = rover.tail
      ret
    }
  }
}

final class Cons[A](hd: A, tl: ImmutableSLList[A]) extends ImmutableSLList[A] {
  def length = 1 + tl.length
  def apply(index: Int): A = if (index == 0) hd else tl.apply(index - 1)
  override def isEmpty = false
  override def head = hd
  override def tail = tl
}

object MyNil extends ImmutableSLList[Nothing] {
  def length = 0
  def apply(index: Int) = throw new IllegalArgumentException("Nil has no contents.")
  override def isEmpty = true
  override def head = throw new IllegalArgumentException("Nil has no head.")
  override def tail = throw new IllegalArgumentException("Nil has no tail.")
}

object ImmutableSLList {
  def apply[A](data: A*): ImmutableSLList[A] = {
    var ret: ImmutableSLList[A] = MyNil
    for (d <- data.reverse) {
      ret = new Cons(d, ret)
    }
    ret
  }
}
