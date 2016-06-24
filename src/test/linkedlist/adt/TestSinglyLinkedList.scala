package test.linkedlist.adt

import org.junit._
import org.junit.Assert._
import linkedlist.adt.SinglyLinkedList

class TestSinglyLinkedList {
  var list: SinglyLinkedList[Int] = null

  @Before def setup: Unit = {
    list = new SinglyLinkedList[Int]
  }

  @Test def appendOne: Unit = {
    list.insert(0, 10)
    assertEquals(10, list(0))
  }

  @Test def appendTwo: Unit = {
    list.insert(0, 10)
    list.insert(0, 20)
    assertEquals(20, list(0))
    assertEquals(10, list(1))
  }

  @Test def appendTwoB: Unit = {
    list.insert(0, 10)
    list.insert(1, 20)
    assertEquals(10, list(0))
    assertEquals(20, list(1))
  }

  @Test def appendTwoUpdate: Unit = {
    list.insert(0, 10)
    list.insert(0, 20)
    list(0) = 5
    list(1) = 8
    assertEquals(5, list(0))
    assertEquals(8, list(1))
  }

  @Test def appendRemove: Unit = {
    list.insert(0, 5)
    list.insert(1, 10)
    list.insert(2, 15)
    list.insert(3, 20)
    assertEquals(5, list(0))
    assertEquals(10, list(1))
    assertEquals(15, list(2))
    assertEquals(20, list(3))
    list.remove(2)
    assertEquals(5, list(0))
    assertEquals(10, list(1))
    assertEquals(20, list(2))
    list.remove(0)
    assertEquals(10, list(0))
    assertEquals(20, list(1))
  }
}