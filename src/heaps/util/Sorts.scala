package heaps.util

object Sorts extends App {
  def heapsort[A](a: Array[A])(comp: (A, A) => Int): Unit = {
    for (end <- 1 until a.length) {
      val tmp = a(end)
      var bubble = end
      while (bubble > 0 && comp(tmp, a((bubble + 1) / 2 - 1)) > 0) {
        a(bubble) = a((bubble + 1) / 2 - 1)
        bubble = (bubble + 1) / 2 - 1
      }
      a(bubble) = tmp
    }
    for (end <- a.length - 1 until 0 by -1) {
      val tmp = a(end)
      a(end) = a(0)
      var stone = 0
      var flag = true
      while (flag && (stone + 1) * 2 - 1 < end) {
        val greaterChild = if ((stone + 1) * 2 < end && comp(a((stone + 1) * 2), a((stone + 1) * 2 - 1)) > 0)
          (stone + 1) * 2 else (stone + 1) * 2 - 1
        if (comp(a(greaterChild), tmp) > 0) {
          a(stone) = a(greaterChild)
          stone = greaterChild
        } else {
          flag = false
        }
      }
      a(stone) = tmp
    }
  }
  
  val arr = Array.fill(10)(math.random)
  heapsort(arr)(_.compareTo(_))
  arr.foreach(println)
}