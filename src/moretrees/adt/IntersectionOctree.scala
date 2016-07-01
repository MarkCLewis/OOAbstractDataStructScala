package moretrees.adt

import collection.mutable

class IntersectionOctree[A <: IntersectionOctree.IntersectionObject](
    objects: Seq[A], centerX: Double, centerY: Double, centerZ: Double,
    treeSize: Double, minSize: Double) {

  private class Node(val cx: Double, val cy: Double, val cz: Double, val size: Double) {
    val objs = mutable.Buffer[A]()
    var children: Array[Node] = null
    var min: Array[Double] = null
    var max: Array[Double] = null

    import IntersectionOctree.ParamCalc

    def add(obj: A): Unit = {
      if (obj.size > size * 0.5 || size > minSize) objs += obj
      else {
        if (children == null) {
          val hsize = size * 0.5
          val qsize = size * 0.25
          children = Array.tabulate(8)(i =>
            new Node(cx - qsize + hsize * (i % 2), cy - qsize + hsize * (i / 2 % 2), cz - qsize + hsize * (i / 4), hsize))
        }
        children(whichChild(obj)).add(obj)
      }
    }

    private def whichChild(obj: A): Int = {
      (if (obj(0) > cx) 1 else 0) + (if (obj(1) > cy) 2 else 0) + (if (obj(2) > cz) 4 else 0)
    }

    def finalizeNode: Unit = {
      if (children != null) children.foreach(_.finalizeNode)
      min = (0 to 2).map(i =>
        objs.view.map(_.min(i)).min min
          (if (children == null) 1e100 else children.view.map(_.min(i)).min)).toArray
      max = (0 to 2).map(i =>
        objs.view.map(_.max(i)).max max
          (if (children == null) -1e100 else children.view.map(_.max(i)).max)).toArray
    }

    def findFirst(pc: ParamCalc[A]): Option[(A, Double)] = {
      val o1 = firstObj(pc)
      val o2 = firstChildObj(pc)
      (o1, o2) match {
        case (None, _) => o2
        case (_, None) => o1
        case (Some((_, p1)), Some((_, p2))) => if (p1 < p2) o1 else o2
      }
    }

    private def firstObj(pc: ParamCalc[A]): Option[(A, Double)] = {
      objs.foldLeft(None: Option[(A, Double)])((opt, obj) => {
        val param = pc(obj)
        if (opt.nonEmpty && opt.get._2 < param) opt else Some(obj, param)
      })
    }

    private def firstChildObj(pc: ParamCalc[A]): Option[(A, Double)] = {
      if (children == null) None else {
        val cparams = for (c <- children; val p = pc(min, max); if c != Double.PositiveInfinity) yield c -> p
        for (i <- 1 until cparams.length) {
          val tmp = cparams(i)
          var j = i - 1
          while (j > 0 && cparams(j)._2 > tmp._2) {
            cparams(j + 1) = cparams(j)
            j -= 1
          }
          cparams(j + 1) = tmp
        }
        var ret: Option[(A, Double)] = None
        var i = 0
        while (i < cparams.length && (ret.isEmpty || cparams(i)._2 > ret.get._2)) {
          val opt = cparams(i)._1.findFirst(pc)
          if (opt.nonEmpty && (ret.isEmpty || opt.get._2 < ret.get._2)) ret = opt
          i += 1
        }
        ret
      }
    }
  }

  private val root = new Node(centerX, centerY, centerZ, treeSize)

  objects.foreach(root.add)
  root.finalizeNode

  def findFirst(pc: IntersectionOctree.ParamCalc[A]): Option[(A, Double)] = {
    root.findFirst(pc)
  }
}

object IntersectionOctree {
  trait IntersectionObject extends (Int => Double) {
    def apply(dim: Int): Double
    def min(dim: Int): Double
    def max(dim: Int): Double
    def size: Double
  }

  /**
   * The A => Double should calculate the intersect parameter for an object.
   * The (Array[Double],Array[Double) => Double should calculate it for a box
   *   with min and max values
   */
  trait ParamCalc[A <: IntersectionOctree.IntersectionObject] extends (A => Double) with ((Array[Double], Array[Double]) => Double)
}