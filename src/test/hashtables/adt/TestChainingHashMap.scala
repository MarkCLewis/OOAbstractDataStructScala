package test.hashtables.adt

import org.junit._
import org.junit.Assert._
import hashtables.adt.ChainingHashMap
import hashtables.adt.HashingMethods

class TestChainingHashMap {
  @Test def emptyOnCreate: Unit = {
    val chm = ChainingHashMap(HashingMethods.Division)
    assertTrue(chm.isEmpty)
  }

  @Test def addGet: Unit = {
    val chm = ChainingHashMap(HashingMethods.Division, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIter: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "5"
    kvSet = kvSet.filter(_._1 != "5")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "8"
    kvSet = kvSet.filter(_._1 != "8")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "1"
    kvSet = kvSet.filter(_._1 != "1")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "6"
    kvSet = kvSet.filter(_._1 != "6")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "15"
    kvSet = kvSet.filter(_._1 != "15")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest3: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap[String, Int](HashingMethods.Division)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap[String, Int](HashingMethods.Division)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }
  
  @Test def testNegativeHashCode: Unit = {
    val chm = ChainingHashMap(HashingMethods.Division, "system" -> 99)
    assertEquals(1, chm.size)
    assertEquals(99, chm("system"))
  }
  
  @Test def testSizeOnDuplicateKeyAdd: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm += "55" -> 1942
    assertEquals(kvSet.size, chm.size)
    assertNotEquals(kvSet, chm.toSet)
  }
  
  @Test def testSizeOnRemoveWithoutKey: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Division, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    chm -= "hi mom"
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def addGetM: Unit = {
    val chm = ChainingHashMap(HashingMethods.Multiplication, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIterM: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = ChainingHashMap(HashingMethods.Multiplication, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1M: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Multiplication, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2M: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Multiplication, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "5"
    kvSet = kvSet.filter(_._1 != "5")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "8"
    kvSet = kvSet.filter(_._1 != "8")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "1"
    kvSet = kvSet.filter(_._1 != "1")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "6"
    kvSet = kvSet.filter(_._1 != "6")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
    chm -= "15"
    kvSet = kvSet.filter(_._1 != "15")
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest3M: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap[String, Int](HashingMethods.Multiplication)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4M: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Multiplication, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5M: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap[String, Int](HashingMethods.Multiplication)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1M: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = ChainingHashMap(HashingMethods.Multiplication, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def testNegativeHashCodeM: Unit = {
    val chm = ChainingHashMap(HashingMethods.Multiplication, "system" -> 99)
    assertEquals(1, chm.size)
    assertEquals(99, chm("system"))
  }
}