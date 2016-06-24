package test.linkedlist.adt

import org.junit._
import org.junit.Assert._
import linkedlist.adt.ListStack
import linkedlist.adt.Stack

class TestListStack {
  var stack: Stack[Int] = null

  @Before def initStack {
    stack = new ListStack[Int]
  }

  @Test def emptyOnCreate {
    assertTrue(stack.isEmpty)
  }

  @Test def nonEmptyOnPush {
    stack.push(5)
    assertFalse(stack.isEmpty)
  }

  @Test def pushPop {
    stack.push(5)
    assertEquals(5, stack.pop)
  }

  @Test def pushPopPushPop {
    stack.push(5)
    assertEquals(5, stack.pop)
    stack.push(3)
    assertEquals(3, stack.pop)
  }

  @Test def pushPushPopPop {
    stack.push(5)
    stack.push(3)
    assertEquals(3, stack.pop)
    assertEquals(5, stack.pop)
  }

  @Test def push100Pop100 {
    val nums = Array.fill(100)(util.Random.nextInt)
    nums.foreach(stack.push(_))
    nums.reverse.foreach(assertEquals(_, stack.pop))
  }
}