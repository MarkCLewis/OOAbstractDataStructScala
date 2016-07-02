package recursionrefresh

import io.StdIn._
import scala.annotation.tailrec

object CodeSegments extends App {
  def fact(n: Int): Int = if (n < 2) 1 else n * fact(n - 1)

  println(fact(5))

  def fact(n: Long): Long = if (n < 2) 1L else n * fact(n - 1)

  println(fact(20L))

  def fact(n: BigInt): BigInt = if (n < 2) 1 else n * fact(n - 1)

  println(fact(BigInt(1000)))

  def countDown(n: Int): Unit = {
    if (n >= 0) {
      println(n)
      countDown(n - 1)
    }
  }

  def countFromTo(from: Int, to: Int): Unit = {
    println(from)
    if (from != to) {
      countFromTo(from + 1, to)
    }
  }

  def countFromTo2(from: Int, to: Int): Unit = {
    println(from)
    if (from != to) {
      countFromTo2(from + (if (from < to) 1 else -1), to)
    }
  }

  def smallest(arr: Array[Int], index: Int): Int = {
    if (index < arr.length) {
      val minimum = smallest(arr, index + 1)
      if (arr(index) <= minimum) {
        arr(index)
      } else {
        minimum
      }
    } else {
      Int.MaxValue
    }
  }

  val arr = Array(4, 2, 9, 8, 3, 5)
  println(smallest(arr, 0))

  def smallest(lst: List[Int]): Int = {
    if (lst.isEmpty) {
      Int.MaxValue
    } else {
      val minimum = smallest(lst.tail)
      if (lst.head <= minimum) {
        lst.head
      } else {
        minimum
      }
    }
  }

  def smallest2(lst: List[Int]): Int = lst match {
    case Nil => Int.MaxValue
    case h :: t => h min smallest2(t)
  }

  def sumInputInts(num: Int): Int = {
    if (num > 0) {
      readInt() + sumInputInts(num - 1)
    } else {
      0
    }
  }

  def sumInputPositive(): Int = {
    val n = readInt()
    if (n > 0) {
      n + sumInputPositive()
    } else {
      0
    }
  }

  def sumUntilQuit(): Int = {
    val n = readLine()
    if (n != "quit") {
      n.toInt + sumUntilQuit()
    } else {
      0
    }
  }

  def sumAndCount(): (Int, Int) = {
    val n = readLine()
    if (n != "quit") {
      val (s, c) = sumAndCount()
      (s + n.toInt, c + 1)
    } else {
      (0, 0)
    }
  }

  def productAndCount(): (Int, Int) = {
    val n = readLine()
    if (n != "quit") {
      val (s, c) = productAndCount()
      (s * n.toInt, c + 1)
    } else {
      (1, 0)
    }
  }

  def inputAndCount(base: Int): (Int, Int) = {
    val n = readLine()
    if (n != "quit") {
      val (s, c) = inputAndCount(base)
      (s * n.toInt, c + 1)
    } else {
      (base, 0)
    }
  }

  def inputAndCount(base: Int, func: (Int, Int) => Int): (Int, Int) = {
    val n = readLine()
    if (n != "quit") {
      val (s, c) = inputAndCount(base, func)
      (func(s, n.toInt), c + 1)
    } else {
      (base, 0)
    }
  }

  @tailrec def sumAndCountTailRec(sum: Int, count: Int): (Int, Int) = {
    val n = readLine()
    if (n != "quit") {
      sumAndCountTailRec(sum + n.toInt, count + 1)
    } else {
      (sum, count)
    }
  }

  def countDown2(n: Int): Unit = n match {
    case 0 =>
    case i =>
      println(i)
      countDown2(i - 1)
  }

  def inputAndCount2(base: Int, func: (Int, Int) => Int): (Int, Int) = readLine() match {
    case "quit" =>
      (base, 0)
    case n =>
      val (s, c) = inputAndCount2(base, func)
      (func(s, n.toInt), c + 1)
  }

  def count(n: Int): Unit = {
    if (n >= 0) {
      count(n - 1)
      println(n)
    }
  }

}