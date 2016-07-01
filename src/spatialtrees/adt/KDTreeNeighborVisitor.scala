package spatialtrees.adt

import scala.reflect.ClassTag

class KDTreeNeighborVisitor[A <% Int => Double: ClassTag](
    d: Int,
    val pIn: IndexedSeq[A]) extends NeighborVisitor[A](d) {

  private val p = pIn.toArray
  private val maxPoints = 3

  class Node
  private class LNode(val pnts: Seq[Int]) extends Node
  private class INode(start: Int, end: Int) extends Node {
    val splitDim = {
      val min = (0 until dim).map(i => p.view.slice(start, end).
        foldLeft(1e100)((m, v) => m min v(i)))
      val max = (0 until dim).map(i => p.view.slice(start, end).
        foldLeft(-1e100)((m, v) => m max v(i)))
      (0 until dim).reduceLeft((i, j) => if (max(j) - min(j) > max(i) - min(i)) j else i)
    }
    val (splitVal, left, right) = {
      val mid = start + (end - start) / 2
      indexPartition(mid, start, end)
      (p(mid)(splitDim), makeChild(start, mid + 1), makeChild(mid + 1, end))
    }
    def indexPartition(index: Int, start: Int, end: Int): Unit = {
      if (end - start > 1) {
        val pivot = if (end - start < 3) start else {
          val mid = start + (end - start) / 2
          val ps = p(start)(splitDim)
          val pm = p(mid)(splitDim)
          val pe = p(end - 1)(splitDim)
          if (ps <= pm && ps >= pe || ps >= pm && ps <= pe) start
          else if (ps <= pm && pm <= pe || ps >= pm && pm >= pe) mid else end - 1
        }
        val ptmp = p(pivot)
        p(pivot) = p(start)
        p(start) = ptmp
        var (low, high) = (start + 1, end - 1)
        while (low <= high) {
          if (p(low)(splitDim) <= ptmp(splitDim)) {
            low += 1
          } else {
            val tmp = p(low)
            p(low) = p(high)
            p(high) = tmp
            high -= 1
          }
        }
        p(start) = p(high)
        p(high) = ptmp
        if (high < index) indexPartition(index, high + 1, end)
        else if (high > index) indexPartition(index, start, high)
      }
    }
    def makeChild(s: Int, e: Int): Node = {
      if (e - s > maxPoints) new INode(s, e) else new LNode(s until e)
    }
  }

  private val root = new INode(0, p.length)

  def visitAllNeighbors(tDist: Double, visit: (A, A) => Unit): Unit = {
    for (i <- 0 until p.length) {
      val pi = p(i)
      def recur(n: Node): Unit = n match {
        case ln: LNode =>
          ln.pnts.foreach(j => if (j > i && dist(p(j), pi) <= tDist) visit(pi, p(j)))
        case in: INode =>
          if (pi(in.splitDim) - tDist <= in.splitVal) recur(in.left)
          if (pi(in.splitDim) + tDist > in.splitVal) recur(in.right)
      }
      recur(root)
    }
  }

  def visitNeighbors(i: Int, tDist: Double, visit: (A, A) => Unit): Unit = {
    val pi = p(i)
    def recur(n: Node): Unit = n match {
      case ln: LNode =>
        ln.pnts.foreach(j => if (j != i && dist(p(j), pi) <= tDist) visit(pi, p(j)))
      case in: INode =>
        if (pi(in.splitDim) - tDist <= in.splitVal) recur(in.left)
        if (pi(in.splitDim) + tDist > in.splitVal) recur(in.right)
    }
    recur(root)
  }
}