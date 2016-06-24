package test.linkedlist.adt

import org.junit._
import org.junit.Assert._
import linkedlist.adt.MyNil
import linkedlist.adt.ImmutableSLList

class TestImmutableSLList {
  @Test def nilIsEmpty: Unit = {
    assertTrue(MyNil.isEmpty)
  }

  @Test def buildCons: Unit = {
    val lst = 1 :: 2 :: 3 :: MyNil
    assertEquals(3, lst.length)
    assertEquals(1, lst.head)
    assertEquals(2, lst(1))
    assertEquals(3, lst(2))
  }

  @Test def buildApply: Unit = {
    val lst = ImmutableSLList(1, 2, 3)
    assertEquals(3, lst.length)
    assertEquals(1, lst.head)
    assertEquals(2, lst(1))
    assertEquals(3, lst(2))
  }
}