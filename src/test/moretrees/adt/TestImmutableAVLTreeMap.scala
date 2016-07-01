package test.moretrees.adt

import org.junit._
import org.junit.Assert._
import moretrees.adt.ImmutableAVLTreeMap

class TestImmutableAVLTreeMap {
  @Test def addGet: Unit = {
    val tm = ImmutableAVLTreeMap("one" -> 1, "two" -> 2, "alpha" -> 0)((s1, s2) => s1.compareTo(s2))
    assertEquals(1, tm("one"))
    assertEquals(2, tm("two"))
    assertEquals(0, tm("alpha"))
  }

  @Test def addIter: Unit = {
    val tm = ImmutableAVLTreeMap("one" -> 1, "two" -> 2, "alpha" -> 0)((s1, s2) => s1.compareTo(s2))
    val ans = Seq("alpha" -> 0, "one" -> 1, "two" -> 2)
    for (((mk, mv), (ak, av)) <- tm zip ans) {
      assertEquals(ak, mk)
      assertEquals(av, mv)
    }
  }

  @Test def bookTest1: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val tm = ImmutableAVLTreeMap(nums.sorted.map(n => n.toString -> n): _*)((s1, s2) => s1.compareTo(s2))
    val snums = nums.sorted
    for ((n, (s, m)) <- snums zip tm) assertEquals(n, m)
  }

  @Test def bookTest2: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val tm = ImmutableAVLTreeMap(nums.sorted.map(n => n.toString -> n): _*)((s1, s2) => s1.compareTo(s2))
    var snums = nums.filterNot(_ == 5).sorted
    val tm2 = tm - "5"
    for ((n, (s, m)) <- snums zip tm2) {
      assertEquals(n, m)
      assertEquals(n.toString, s)
    }
    snums = snums.filterNot(_ == 8).sorted
    val tm3 = tm2 - "8"
    for ((n, (s, m)) <- snums zip tm3) {
      assertEquals(n, m)
      assertEquals(n.toString, s)
    }
    snums = snums.filterNot(_ == 1).sorted
    val tm4 = tm3 - "1"
    for ((n, (s, m)) <- snums zip tm4) {
      assertEquals(n, m)
      assertEquals(n.toString, s)
    }
    snums = snums.filterNot(_ == 6).sorted
    val tm5 = tm4 - "6"
    for ((n, (s, m)) <- snums zip tm5) {
      assertEquals(n, m)
      assertEquals(n.toString, s)
    }
  }

  @Test def bookTest3: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var tm = ImmutableAVLTreeMap[String, Int]()((s1, s2) => s1.compareTo(s2))
    for (n <- nums) tm += n.toString -> n
    val snums = nums.sorted
    for ((n, (s, m)) <- snums zip tm) assertEquals(n, m)
  }

  @Test def bookTest4: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    var tm = ImmutableAVLTreeMap(nums.sorted.map(n => n.toString -> n): _*)((s1, s2) => s1.compareTo(s2))
    val snums = nums.sorted
    for ((n, (s, m)) <- snums zip tm) assertEquals(n, m)
  }

  @Test def bookTest5: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    var tm = ImmutableAVLTreeMap[String, Int]()((s1, s2) => s1.compareTo(s2))
    for (n <- nums) tm += n.toString -> n
    val snums = nums.sorted
    for ((n, (s, m)) <- snums zip tm) assertEquals(n, m)
    assertTrue(tm.verifyBalance)
  }

  @Test def bookTest6: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    var tm = ImmutableAVLTreeMap[String, Int]()((s1, s2) => s1.compareTo(s2))
    for (n <- nums) tm += n.toString -> n
    val snums = nums.sorted
    for ((n, (s, m)) <- snums zip tm) assertEquals(n, m)
    assertTrue(tm.verifyBalance)
    tm -= "1"
    tm -= "2"
    tm -= "3"
    assertTrue(tm.verifyBalance)
  }
}