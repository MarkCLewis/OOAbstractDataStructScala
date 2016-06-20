package multithreading2

import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object ParallelFactorials extends App {
  def fact(n: BigInt) = (BigInt(1) to n).product

  def parallelFactorial(n: BigInt, es: ExecutorService, nThreads: Int): BigInt = {
    val block = n / nThreads
    val rem = n % nThreads
    var i = BigInt(1)
    val futures = Array.tabulate(nThreads)(j => es.submit(new Callable[BigInt] {
      def call(): BigInt = {
        val start = i
        val end = start + block + (if (BigInt(j) < rem) 1 else 0)
        i = end
        (start until end).product
      }
    }))
    futures.map(_.get).product
  }

  def parallelFactorial2(n: BigInt, es: ExecutorService, nThreads: Int): BigInt = {
    val futures = Array.tabulate(nThreads)(j => es.submit(new Callable[BigInt] {
      def call(): BigInt = {
        (BigInt(j + 1) to n by nThreads).product
      }
    }))
    futures.map(_.get).product
  }

  def parallelFactorial(n: BigInt, nThreads: Int): Future[BigInt] = {
    val futures = List.tabulate(nThreads)(j => Future {
      (BigInt(j + 1) to n by nThreads).product
    })
    Future.sequence(futures).map(_.product)
  }

  def parallelFactorial(n: BigInt) = (BigInt(1) to n).par.product

  // Speed testing
  val es = Executors.newCachedThreadPool()
  multithreading1.RandomCodeSegments.timeCode(fact(30000))
  multithreading1.RandomCodeSegments.timeCode(parallelFactorial(30000, es, 4))
  multithreading1.RandomCodeSegments.timeCode(parallelFactorial2(30000, es, 4))
  multithreading1.RandomCodeSegments.timeCode(Await.result(parallelFactorial(30000, 4), 10.seconds))
  multithreading1.RandomCodeSegments.timeCode(parallelFactorial(30000))
  es.shutdown()
  
}