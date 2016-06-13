package polymorphism

object Sorting {
  def bubbleSort(arr: Array[Double]): Unit = {
    for (i <- 0 until arr.length - 1; j <- 0 until arr.length - i - 1) {
      if (arr(j + 1) < arr(j)) {
        val tmp = arr(j)
        arr(j) = arr(j + 1)
        arr(j + 1) = tmp
      }
    }
  }
}