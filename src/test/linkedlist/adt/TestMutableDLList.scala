package test.linkedlist.adt

import org.junit._
import org.junit.Assert._
import linkedlist.adt.MutableDLList

class TestMutableDLList {
  var list: MutableDLList[Int] = null

  @Before def setup {
    list = new MutableDLList[Int]
  }

  @Test def startEmpty {
    assertTrue(list.isEmpty)
  }

  @Test def appendOne {
    list += 10
    assertFalse(list.isEmpty)
    assertEquals(1, list.length)
    assertEquals(10, list(0))
  }

  @Test def prependOne {
    10 +=: list
    assertFalse(list.isEmpty)
    assertEquals(1, list.length)
    assertEquals(10, list(0))
  }

  @Test def appendOnePrependOne {
    list += 5
    10 +=: list
    assertFalse(list.isEmpty)
    assertEquals(2, list.length)
    assertEquals(10, list(0))
    assertEquals(5, list(1))
  }

  @Test def insertList {
    list += 5 += 10
    list.insertAll(0, List(1, 2, 3))
    for ((e, a) <- List(1, 2, 3, 5, 10).zip(list)) {
      assertEquals(e, a)
    }
  }

  @Test def insertListRemove {
    list += 5 += 10
    list.insertAll(0, List(1, 2, 3))
    assertEquals(3, list.remove(2))
    for ((e, a) <- List(1, 2, 5, 10).zip(list)) {
      assertEquals(e, a)
    }
  }
}