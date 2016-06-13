package polymorphism

import io.StdIn._

object BasicAbstraction {
  def square(x: Double): Double = x * x

  def sumValues(n: Int): Double = {
    if (n < 1) 0.0 else readDouble() + sumValues(n - 1)
  }

  def multiplyValues(n: Int): Double = {
    if (n < 1) 1.0 else readDouble() * multiplyValues(n - 1)
  }

  def combineValues(n: Int, base: Double, op: (Double, Double) => Double): Double = {
    if (n < 1) base else op(readDouble(), combineValues(n - 1, base, op))
  }

  def main(args: Array[String]): Unit = {
    println(combineValues(5, Double.MaxValue, _ min _))
  }
}