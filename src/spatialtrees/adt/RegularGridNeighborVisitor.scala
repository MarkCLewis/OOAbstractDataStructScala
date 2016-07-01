package spatialtrees.adt

import collection.mutable

class RegularGridNeighborVisitor[A <% Int => Double](
    val p: IndexedSeq[A]) extends NeighborVisitor[A](2) {

  private val grid = mutable.ArrayBuffer.fill(1, 1)(mutable.Buffer[Int]())
  private var currentSpacing = 0.0
  private var min = (0 until dim).map(i => p.foldLeft(1e100)((d, p) => d min p(i)))
  private var max = (0 until dim).map(i => p.foldLeft(-1e100)((d, p) => d max p(i)))

  def visitAllNeighbors(tDist: Double, visit: (A, A) => Unit): Unit = {
    if (tDist < 0.5 * currentSpacing || tDist > 5 * currentSpacing) rebuildGrid(tDist)
    val mult = math.ceil(tDist / currentSpacing).toInt
    val offsets = Array.tabulate(2 * mult + 1, 2 * mult + 1)((i, j) => (i - mult, j - mult)).
      flatMap(i => i).filter(t => t._2 > 0 || t._2 == 0 && t._1 >= 0)
    for {
      cx <- grid.indices
      cy <- grid(cx).indices
      (dx, dy) <- offsets
      val gx = cx + dx
      val gy = cy + dy
      if gx >= 0 && gx < grid.length && gy >= 0 && gy < grid(gx).length
      i <- grid(cx)(cy).indices
      val pi = p(grid(cx)(cy)(i))
    } {
      if (dx == 0 && dy == 0) {
        for {
          j <- i + 1 until grid(cx)(cy).length
          val pj = p(grid(cx)(cy)(j))
          if dist(pi, pj) <= tDist
        } visit(pi, pj)
      } else {
        for {
          j <- grid(gx)(gy)
          val pj = p(j)
          if dist(pi, pj) <= tDist
        } visit(pi, pj)
      }
    }
  }

  def visitNeighbors(i: Int, tDist: Double, visit: (A, A) => Unit): Unit = {
    if (tDist < 0.5 * currentSpacing || tDist > 5 * currentSpacing) rebuildGrid(tDist)
    val mult = math.ceil(tDist / currentSpacing).toInt
    val offsets = Array.tabulate(2 * mult + 1, 2 * mult + 1)((i, j) => (i - mult, j - mult)).
      flatMap(i => i)
    val cx = ((p(i)(0) - min(0)) / currentSpacing).toInt
    val cy = ((p(i)(1) - min(1)) / currentSpacing).toInt
    val pi = p(i)
    for {
      (dx, dy) <- offsets
      val gx = cx + dx
      val gy = cy + dy
      if gx >= 0 && gx < grid.length && gy >= 0 && gy < grid(gx).length
      j <- grid(gx)(gy)
      if i != j
      val pj = p(j)
      if dist(pi, pj) <= tDist
    } visit(pi, pj)
  }

  /**
   * Rebuild the grid to a size that is appropriate for the searches. Note that
   * this method was not written for efficiency. A true implementation should
   * include a rewrite of this method.
   */
  private def rebuildGrid(spacing: Double): Unit = {
    min = (0 until dim).map(i => p.foldLeft(1e100)((d, p) => d min p(i)))
    max = (0 until dim).map(i => p.foldLeft(-1e100)((d, p) => d max p(i)))
    val cells = (0 until dim).map(i => ((max(i) - min(i)) / spacing).toInt + 1)
    if (grid.size < cells(0)) grid.append(mutable.Buffer.fill(cells(0) - grid.size)(mutable.ArrayBuffer.fill(cells(1))(mutable.Buffer[Int]())): _*)
    else if (grid.size > cells(0)) grid.trimEnd(grid.size - cells(0))
    for (col <- grid) {
      if (col.size < cells(1)) col.append(mutable.Buffer.fill(cells(1) - col.size)(mutable.Buffer[Int]()): _*)
      else if (col.size > cells(1)) col.trimEnd(col.size - cells(1))
      col.foreach(_.clear)
    }
    for (i <- p.indices) {
      val cx = ((p(i)(0) - min(0)) / spacing).toInt
      val cy = ((p(i)(1) - min(1)) / spacing).toInt
      grid(cx)(cy) += i
    }
    currentSpacing = spacing
  }
}