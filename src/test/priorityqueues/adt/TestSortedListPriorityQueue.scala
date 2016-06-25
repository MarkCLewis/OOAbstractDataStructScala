package test.priorityqueues.adt

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

import priorityqueues.adt.PriorityQueue
import priorityqueues.adt.SortedListPriorityQueue

class TestSortedListPriorityQueue {
  var queue:PriorityQueue[Int] = null
  
  @Before def initQueue {
    queue = new SortedListPriorityQueue[Int]((a,b) => a.compareTo(b))
  }
  
  @Test def emptyOnCreate {
    assertTrue(queue.isEmpty)
  }
  
  @Test def nonEmptyOnEnqueue {
    queue.enqueue(5)
    assertFalse(queue.isEmpty)
  }
  
  @Test def enqueueDequeue {
    queue.enqueue(5)
    assertEquals(5,queue.dequeue)
  }
  
  @Test def enqueueDequeueEnqueueDequeue {
    queue.enqueue(5)
    assertEquals(5,queue.dequeue)
    queue.enqueue(3)
    assertEquals(3,queue.dequeue)
  }

  @Test def enqueueEnqueueDequeueDequeue {
    queue.enqueue(5)
    queue.enqueue(3)
    assertEquals(5,queue.dequeue)
    assertEquals(3,queue.dequeue)
  }
  
  @Test def enqueue100Dequeue100 {
    val nums = Array.fill(100)(util.Random.nextInt)
    nums.foreach(queue.enqueue)
    nums.sorted.reverse.foreach(assertEquals(_,queue.dequeue))
  }
}