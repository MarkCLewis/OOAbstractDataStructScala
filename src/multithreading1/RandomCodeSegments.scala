package multithreading1

import scala.concurrent.Await
import scala.concurrent.blocking
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.collection.parallel.mutable.ParArray

object RandomCodeSegments extends App {
  def timeCode[A](body: => A): A = {
    val n = 10
    for (i <- 1 to n) body
    val start = System.nanoTime()
    for (i <- 1 to n-1) body
    val ret = body
    println((System.nanoTime() - start) / 1e9 / n)
    ret
  }

  println("Start Futures")

  // Futures with for loops
  val f1 = Future { slowMethodProducingString() }
  val f2 = Future { slowMethodProducingInt() }
  val f3 = Future { slowMethodProducingListInt() }
  val f4 = for {
    str <- f1
    i <- f2
    lst <- f3
  } yield {
    combineResults(str, i, lst)
  }

  // And without the for
  val f5 = f1.flatMap(str => f2.flatMap(i => f3.map(lst => combineResults(str, i, lst))))

  // Await.result
  val answer = Await.result(f4, 10.seconds)

  // blocking
  val word = blocking { io.StdIn.readLine() }

  println("Start par collections")

  // Fibonacci
  def fib(n: Int): Int = if (n < 2) 1 else fib(n - 1) + fib(n - 2)

  timeCode {
    for (n <- (35 to 15 by -1).par) println(fib(n))
  }

  // Aggregate
  val words = makeReallyBigArrayOfStrings()
  val sumLengths = words.par.aggregate(0)((lenSum, str) => lenSum + str.length, (len1, len2) => len1 + len2)
  val sumLengths2 = words.par.aggregate(0)(_ + _.length, _ + _)

  val data = ParArray.fill(1000)(math.random)
  val (min, max) = data.aggregate((data(0), data(0)))(
    (mm, n) => (n min mm._1, n max mm._2),
    (mm1, mm2) => (mm1._1 min mm2._1, mm1._2 max mm2._2))

  // Race conditions
  var cnt = 0
  for (i <- (1 to 1000000000).par) cnt += 1
  println(cnt)

  val account = new Account(0, "01234567")
  for (i <- (1 to 10000000).par) account.deposit(1)
  println(account.balance)

  var cnt2 = 0L
  for (i <- (1 to 1000000000).par) cnt2 += someFunction(i)
  println(cnt2)

  val cnt3 = (for (i <- (1 to 10000000).par) yield someFunction(i)).sum
  val cnt4 = (1 to 10000000).par.map(someFunction).sum

  // Deadlock
  def method1(a: A, b: B): Unit = {
    a.synchronized {
      // Do stuff - 1
      b.synchronized {
        // Do Stuff - 2
      }
      // Do Stuff - 3
    }
  }
  
  def method2(a: A, b: B): Unit = {
    b.synchronized {
      // Do stuff - A
      a.synchronized {
        // Do Stuff - B
      }
      // Do Stuff - C
    }
  }
  
  println(Thread.currentThread().getName)
  
  // Methods written to make the above code compile.
  def slowMethodProducingString(): String = "hi"
  def slowMethodProducingInt(): Int = 42
  def slowMethodProducingListInt(): List[Int] = List(3, 1, 4, 1, 5, 9)
  def combineResults(s: String, i: Int, lst: List[Int]): Double = 0.0
  def makeReallyBigArrayOfStrings(): Array[String] = "Hi there".split(" ")
  def someFunction(i:Int):Int = 1
  
  class A
  class B
}