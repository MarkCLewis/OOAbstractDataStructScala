package spatialtrees.adt

abstract class NeighborVisitor[A <% Int => Double](val dim: Int) {
  def visitAllNeighbors(tDist: Double, visit: (A, A) => Unit): Unit
  def visitNeighbors(i: Int, tDist: Double, visit: (A, A) => Unit): Unit

  var distCalcs = 0
  def dist[A <% Int => Double](p1: A, p2: A): Double = {
    distCalcs += 1
    math.sqrt((0 until dim).foldLeft(0.0)((d, i) => {
      val di = p1(i) - p2(i)
      d + di * di
    }))
  }
}