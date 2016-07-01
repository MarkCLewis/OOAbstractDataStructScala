package spatialtrees.adt

class BruteForceNeighborVisitor[A <% Int => Double](
    d: Int,
    val p: IndexedSeq[A]) extends NeighborVisitor[A](d) {

  def visitAllNeighbors(tDist: Double, visit: (A, A) => Unit): Unit = {
    for {
      i <- 0 until p.length
      val pi = p(i)
      j <- i + 1 until p.length
      val pj = p(j)
      if dist(pi, pj) <= tDist
    } visit(pi, pj)
  }

  def visitNeighbors(i: Int, tDist: Double, visit: (A, A) => Unit): Unit = {
    val pi = p(i)
    for {
      j <- 0 until p.length
      if j != i
      val pj = p(j)
      if dist(pi, pj) <= tDist
    } visit(pi, pj)
  }
}