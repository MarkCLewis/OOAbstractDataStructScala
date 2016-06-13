package polymorphism

object Sorts {
  //  def bubbleSort[A](arr: Array[A]): Unit = {
  //    for (i <- 0 until arr.length - 1; j <- 0 until arr.length - i - 1) {
  //      if (arr(j + 1) < arr(j)) { // Error: < not a member of type parameter A
  //        val tmp = arr(j)
  //        arr(j) = arr(j + 1)
  //        arr(j + 1) = tmp
  //      }
  //    }
  //  }

  def bubbleSort[A](a: Array[A])(lt: (A, A) => Boolean) {
    for (i <- 0 until a.length; j <- 0 until a.length - 1 - i) {
      if (lt(a(j + 1), a(j))) {
        val tmp = a(j)
        a(j) = a(j + 1)
        a(j + 1) = tmp
      }
    }
  }

  def bubbleSort[A <% Ordered[A]](a: Array[A]) {
    for (i <- 0 until a.length; j <- 0 until a.length - 1 - i) {
      if (a(j + 1) < a(j)) {
        val tmp = a(j)
        a(j) = a(j + 1)
        a(j + 1) = tmp
      }
    }
  }

}