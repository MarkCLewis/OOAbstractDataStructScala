package moretrees.adt

object RayTracing extends App {
  case class Ray(p0: Array[Double], d: Array[Double]) extends IntersectionOctree.ParamCalc[Sphere] {
    def apply(s: Sphere): Double = {
      val dist = (p0 zip s.c).map(t => t._1 - t._2)
      val a = d(0) * d(0) + d(1) * d(1) + d(2) * d(2)
      val b = 2 * (d(0) * dist(0) + d(1) * dist(1) + d(2) * dist(2))
      val c = (dist(0) * dist(0) + dist(1) * dist(1) + dist(2) * dist(2)) - s.rad * s.rad
      val root = b * b - 4 * a * c
      if (root < 0) Double.PositiveInfinity
      else (-b - math.sqrt(root)) / (2 * a)
    }

    def apply(min: Array[Double], max: Array[Double]): Double = {
      if ((p0, min, max).zipped.forall((p, l, h) => p >= l && p <= h)) 0.0 else {
        val minp = (0 to 2).map(i => (min(i) - p0(i)) / d(i))
        val maxp = (0 to 2).map(i => (max(i) - p0(i)) / d(i))
        var ret = Double.PositiveInfinity
        for (i <- 0 to 2) {
          val first = minp(i) min maxp(i)
          if (first >= 0 && first < ret) {
            val (j, k) = ((i + 1) % 3, (i + 2) % 3)
            if (first >= (minp(j) min maxp(j)) && first <= (minp(j) max maxp(j)) &&
              first >= (minp(k) min maxp(k)) && first <= (minp(k) max maxp(k))) ret = first
          }
        }
        ret
      }
    }
  }

  class Sphere(val c: Array[Double], val rad: Double) extends IntersectionOctree.IntersectionObject {
    def apply(dim: Int): Double = c(dim)
    def min(dim: Int): Double = c(dim) - rad
    def max(dim: Int): Double = c(dim) + rad
    def size: Double = rad * 2
  }
}