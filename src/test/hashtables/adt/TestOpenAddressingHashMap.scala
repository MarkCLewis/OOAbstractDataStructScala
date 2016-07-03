package test.hashtables.adt

import org.junit._
import org.junit.Assert._
import hashtables.adt.ChainingHashMap
import hashtables.adt.OpenAddressingHashMap
import hashtables.adt.HashingMethods
import hashtables.adt.ProbingMethods

class TestOpenAddressingHashMap {
  @Test def emptyOnCreate: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear)
    assertTrue(chm.isEmpty)
  }

  @Test def addGet: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIter: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, kvSet.toSeq: _*)
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
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Division, ProbingMethods.Linear)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Division, ProbingMethods.Linear)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def addGetM: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIterM: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1M: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2M: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, kvSet.toSeq: _*)
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
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Multiplication, ProbingMethods.Linear)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4M: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5M: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Multiplication, ProbingMethods.Linear)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1M: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Linear, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def emptyOnCreateQ: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic)
    assertTrue(chm.isEmpty)
  }

  @Test def addGetQ: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIterQ: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1Q: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2Q: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, kvSet.toSeq: _*)
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

  @Test def bookTest3Q: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Division, ProbingMethods.Quadratic)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4Q: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5Q: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Division, ProbingMethods.Quadratic)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1Q: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Division, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def addGetMQ: Unit = {
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, "one" -> 1, "two" -> 2, "alpha" -> 0)
    assertEquals(1, chm("one"))
    assertEquals(2, chm("two"))
    assertEquals(0, chm("alpha"))
  }

  @Test def addIterMQ: Unit = {
    val kvSet = Set("one" -> 1, "two" -> 2, "alpha" -> 0)
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest1MQ: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest2MQ: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    var kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, kvSet.toSeq: _*)
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

  @Test def bookTest3MQ: Unit = {
    val nums = Array(5, 3, 1, 8, 7, 4, 2, 9, 0, 6)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Multiplication, ProbingMethods.Quadratic)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest4MQ: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, nums.sorted.map(n => n.toString -> n): _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bookTest5MQ: Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap[String, Int](HashingMethods.Multiplication, ProbingMethods.Quadratic)
    for (n <- nums) chm += n.toString -> n
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }

  @Test def bigTest1MQ: Unit = {
    val nums = 1 to 100
    val kvSet = nums.sorted.map(n => n.toString -> n).toSet
    val chm = OpenAddressingHashMap(HashingMethods.Multiplication, ProbingMethods.Quadratic, kvSet.toSeq: _*)
    assertEquals(kvSet.size, chm.size)
    assertEquals(kvSet, chm.toSet)
  }
}