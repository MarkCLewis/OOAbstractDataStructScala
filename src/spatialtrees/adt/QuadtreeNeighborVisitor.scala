package spatialtrees.adt

class QuadtreeNeighborVisitor[A <% Int => Double](
    val p: IndexedSeq[A]) extends NeighborVisitor[A](2) {

  private class Node(val cx: Double, val cy: Double, val size: Double)
  private class LNode(x: Double, y: Double, s: Double, val pnts: IndexedSeq[Int]) extends Node(x, y, s)
  private class INode(x: Double, y: Double, s: Double, pnts: IndexedSeq[Int]) extends Node(x, y, s) {
    val children = {
      val groups = pnts.groupBy(pn => childNum(p(pn)(0), p(pn)(1)))
      val hs = s * 0.5
      val qs = s * 0.25
      val ox = cx - qs
      val oy = cy - qs
      Array.tabulate(4)(i => makeChild(ox + hs * (i % 2), oy + hs * (i / 2), hs,
        if (groups.contains(i)) groups(i) else IndexedSeq[Int]()))
    }
    def makeChild(x: Double, y: Double, s: Double, pnts: IndexedSeq[Int]): Node =
      if (pnts.length > maxPoints) new INode(x, y, s, pnts)
      else new LNode(x, y, s, pnts)
    def childNum(x: Double, y: Double): Int = (if (x > cx) 1 else 0) + (if (y > cy) 2 else 0)
  }

  private val maxPoints = 1
  private val root = {
    val minx = p.foldLeft(1e100)((mx, pnt) => pnt(0) min mx)
    val maxx = p.foldLeft(-1e100)((mx, pnt) => pnt(0) max mx)
    val miny = p.foldLeft(1e100)((my, pnt) => pnt(1) min my)
    val maxy = p.foldLeft(-1e100)((my, pnt) => pnt(1) max my)
    val cx = 0.5 * (minx + maxx)
    val cy = 0.5 * (miny + maxy)
    val s = (maxx - minx) max (maxy - miny)
    if (p.length > maxPoints) new INode(cx, cy, s, p.indices)
    else new LNode(cx, cy, s, p.indices)
  }

  def visitAllNeighbors(tDist: Double, visit: (A, A) => Unit): Unit = {
    for (i <- 0 until p.length) {
      val pi = p(i)
      def recur(n: Node): Unit = n match {
        case ln: LNode =>
          ln.pnts.filter(j => j > i && dist(pi, p(j)) <= tDist).foreach(j => visit(pi, p(j)))
        case in: INode =>
          val x = pi(0)
          val y = pi(1)
          if (x + tDist > in.cx) {
            if (y + tDist > in.cy) recur(in.children(3))
            if (y - tDist <= in.cy) recur(in.children(1))
          }
          if (x - tDist <= in.cx) {
            if (y + tDist > in.cy) recur(in.children(2))
            if (y - tDist <= in.cy) recur(in.children(0))
          }
      }
      recur(root)
    }
  }

  def visitNeighbors(i: Int, tDist: Double, visit: (A, A) => Unit): Unit = {
    val pi = p(i)
    def recur(n: Node): Unit = n match {
      case ln: LNode =>
        ln.pnts.filter(j => j != i && dist(pi, p(j)) <= tDist).foreach(j => visit(pi, p(j)))
      case in: INode =>
        val x = pi(0)
        val y = pi(1)
        if (x + tDist > in.cx) {
          if (y + tDist > in.cy) recur(in.children(3))
          if (y - tDist <= in.cy) recur(in.children(1))
        }
        if (x - tDist <= in.cx) {
          if (y + tDist > in.cy) recur(in.children(0))
          if (y - tDist <= in.cy) recur(in.children(2))
        }
    }
    recur(root)
  }
}