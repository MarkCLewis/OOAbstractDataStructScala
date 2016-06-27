package recursion.util

import scala.reflect.ClassTag

object Sorting extends App {
  def mergeSort[A](lst: List[A])(comp: (A, A) => Int): List[A] = {
    def merge(l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (_, _) =>
        if (comp(l1.head, l2.head) <= 0) l1.head :: merge(l1.tail, l2)
        else l2.head :: merge(l1, l2.tail)
    }

    val len = lst.length
    if (len < 2) lst
    else {
      val (front, back) = lst.splitAt(len / 2)
      merge(mergeSort(front)(comp), mergeSort(back)(comp))
    }
  }

  def mergeSort2[A](lst: List[A])(comp: (A, A) => Int): List[A] = {
    def merge(l1: List[A], l2: List[A]): List[A] = {
      var (lst1, lst2, ret) = (l1, l2, List[A]())
      while (lst1.nonEmpty || lst2.nonEmpty) {
        if (lst2.isEmpty || (lst1.nonEmpty && comp(lst1.head, lst2.head) <= 0)) {
          ret ::= lst1.head
          lst1 = lst1.tail
        } else {
          ret ::= lst2.head
          lst2 = lst2.tail
        }
      }
      ret.reverse
    }

    val len = lst.length
    if (len < 2) lst
    else {
      val (front, back) = lst.splitAt(len / 2)
      merge(mergeSort2(front)(comp), mergeSort2(back)(comp))
    }
  }

  def mergeSort[A: ClassTag](a: Array[A])(comp: (A, A) => Int) {
    val data = Array(a, a.map(i => i))
    def mergeSortRecur[A](start: Int, end: Int, dest: Int) {
      val src = 1 - dest
      if (start == end - 1) {
        if (dest == 1) data(dest)(start) = data(src)(start)
      } else {
        val mid = (start + end) / 2 // Can fail for arrays over 2^30 in length
        mergeSortRecur(start, mid, src)
        mergeSortRecur(mid, end, src)
        var (p1, p2, pdest) = (start, mid, start)
        while (pdest < end) {
          if ((p2 >= end || comp(data(src)(p1), data(src)(p2)) <= 0) && p1 < mid) {
            data(dest)(pdest) = data(src)(p1)
            p1 += 1
          } else {
            data(dest)(pdest) = data(src)(p2)
            p2 += 1
          }
          pdest += 1
        }
      }
    }
    mergeSortRecur(0, a.length, 0)
  }
  
  def quicksort[A](lst: List[A])(lt: (A, A) => Boolean):List[A] = lst match {
    case Nil => lst
    case h::Nil => lst
    case pivot::t =>
      val (before, after) = t.partition(a => lt(a, pivot))
      quicksort(before)(lt) ::: (pivot :: quicksort(after)(lt))
  }

  def quicksort[A](a: Array[A])(comp: (A, A) => Int) {
    //    def pickPivot(start:Int,end:Int) = start
    def pickPivot(start: Int, end: Int) = start + util.Random.nextInt(end - start)

    def qsRecur(start: Int, end: Int) {
      if (start < end - 1) {
        val pivot = pickPivot(start, end)
        val p = a(pivot)
        a(pivot) = a(start)
        a(start) = p
        var (low, high) = (start + 1, end - 1)
        while (low <= high) {
          if (comp(a(low), p) <= 0) {
            low += 1
          } else {
            val tmp = a(low)
            a(low) = a(high)
            a(high) = tmp
            high -= 1
          }
        }
        a(start) = a(high)
        a(high) = p
        qsRecur(start, high)
        qsRecur(low, end)
      }
    }
    qsRecur(0, a.length)
  }

  def quicksort2[A](a: Array[A])(comp: (A, A) => Int) {
    def insertionSort(start: Int, end: Int) {
      for (i <- start + 1 until end) {
        val tmp = a(i)
        var j = i - 1
        while (j >= 0 && comp(a(j), tmp) > 0) {
          a(j + 1) = a(j)
          j -= 1
        }
        a(j + 1) = tmp
      }
    }

    def pickPivot(start: Int, end: Int) = {
      val mid = start + (end - start) / 2
      val sm = comp(a(start), a(mid))
      val se = comp(a(start), a(end - 1))
      if (sm <= 0 && se >= 0 || sm >= 0 && se <= 0) start
      else {
        val me = comp(a(mid), a(end - 1))
        if (sm <= 0 && me <= 0 || sm >= 0 && me >= 0) mid else end - 1
      }
    }

    def qsRecur(start: Int, end: Int) {
      if (start < end - 7) {
        val pivot = pickPivot(start, end)
        val p = a(pivot)
        a(pivot) = a(start)
        a(start) = p
        var (low, high) = (start + 1, end - 1)
        while (low <= high) {
          if (comp(a(low), p) <= 0) {
            low += 1
          } else {
            val tmp = a(low)
            a(low) = a(high)
            a(high) = tmp
            high -= 1
          }
        }
        a(start) = a(high)
        a(high) = p
        qsRecur(start, high)
        qsRecur(low, end)
      } else {
        insertionSort(start, end)
      }
    }
    qsRecur(0, a.length)
  }

  var cnt = 0
  for (i <- 1 to 100) {
    val arr = Array.fill(10000)(math.random)
    quicksort2(arr)((a, b) => { cnt += 1; a.compareTo(b) })
  }
  println(cnt)
}