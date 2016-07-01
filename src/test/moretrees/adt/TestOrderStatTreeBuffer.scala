package test.moretrees.adt

import org.junit._
import org.junit.Assert._
import moretrees.adt.OrderStatTreeBuffer

class TestOrderStatTreeBuffer {
  var buf: OrderStatTreeBuffer[Int] = null

  @Before def setup: Unit = {
    buf = new OrderStatTreeBuffer[Int]
  }

  @Test def startEmpty: Unit = {
    assertTrue(buf.isEmpty)
  }

  @Test def appendOne: Unit = {
    buf += 10
    assertFalse(buf.isEmpty)
    assertEquals(1, buf.length)
    assertEquals(10, buf(0))
  }

  @Test def prependOne: Unit = {
    10 +=: buf
    assertFalse(buf.isEmpty)
    assertEquals(1, buf.length)
    assertEquals(10, buf(0))
  }

  @Test def appendOnePrependOne: Unit = {
    buf += 5
    10 +=: buf
    assertFalse(buf.isEmpty)
    assertEquals(2, buf.length)
    assertEquals(10, buf(0))
    assertEquals(5, buf(1))
  }

  @Test def insertList: Unit = {
    buf += 5 += 10
    buf.insertAll(0, List(1, 2, 3))
    for ((e, a) <- List(1, 2, 3, 5, 10).zip(buf)) {
      assertEquals(e, a)
    }
  }

  @Test def insertListRemove: Unit = {
    buf += 5 += 10
    buf.insertAll(0, List(1, 2, 3))
    assertEquals(3, buf.remove(2))
    for ((e, a) <- List(1, 2, 5, 10).zip(buf)) {
      assertEquals(e, a)
    }
  }

  @Test def insertListMid: Unit = {
    buf += 5 += 10 += 20
    buf.insertAll(1, List(1, 2, 3))
    for ((e, a) <- List(5, 1, 2, 3, 10, 20).zip(buf)) {
      assertEquals(e, a)
    }
  }

  @Test def insertListMidRemove: Unit = {
    buf += 5 += 10 += 20
    buf.insertAll(1, List(1, 2, 3))
    for ((e, a) <- List(5, 1, 2, 3, 10, 20).zip(buf)) {
      assertEquals(e, a)
    }
    buf.remove(2)
    for ((e, a) <- List(5, 1, 3, 10, 20).zip(buf)) {
      assertEquals(e, a)
    }
    buf.remove(2)
    for ((e, a) <- List(5, 1, 10, 20).zip(buf)) {
      assertEquals(e, a)
    }
    buf.remove(2)
    for ((e, a) <- List(5, 1, 20).zip(buf)) {
      assertEquals(e, a)
    }
    buf.remove(2)
    for ((e, a) <- List(5, 1).zip(buf)) {
      assertEquals(e, a)
    }
  }

  @Test def bigTest: Unit = {
    val comp = collection.mutable.Buffer.fill(1000)(util.Random.nextInt)
    comp foreach (buf += _)
    assertEquals(comp, buf)
    assertEquals(comp.size, buf.size)
    for (i <- 1 to 200) {
      val index = util.Random.nextInt(comp.length)
      comp.remove(index)
      buf.remove(index)
      assertEquals(comp.size, buf.size)
    }
    assertEquals(comp, buf)
    for (i <- 1 to 200) {
      val index = util.Random.nextInt(comp.length)
      comp.insert(index, util.Random.nextInt)
      buf.insert(index, comp(index))
      assertEquals(comp.size, buf.size)
    }
    assertEquals(comp, buf)
  }
}