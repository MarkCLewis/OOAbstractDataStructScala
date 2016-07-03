package hashtables.adt

import collection.mutable

class ChainingHashMap[K, V](method: HashingMethods.Value) extends mutable.Map[K, V] {
  private var numElems = 0
  private var table = Array.fill(11)(List[(K, V)]())
  private val hashFunc = HashingMethods.buildHashFunction(method)

  def +=(kv: (K, V)): ChainingHashMap.this.type = {
    def appendOrUpdate(lst: List[(K, V)]): (Boolean, List[(K, V)]) = lst match {
      case Nil => (true, List(kv))
      case h :: t => if (h._1 == kv._1) (false, kv :: t) else {
        val (added, newt) = appendOrUpdate(t)
        (added, h :: newt)
      }
    }

    if (numElems > ChainingHashMap.fillingFactor * table.length) growTable()
    val index = hashFunc(kv._1, table.length)
    val (added, lst) = appendOrUpdate(table(index))
    table(index) = lst
    if(added) numElems += 1
    this
  }

  def -=(key: K): ChainingHashMap.this.type = {
    def removeMatch(lst: List[(K, V)]): (Boolean, List[(K, V)]) = lst match {
      case Nil => (false, Nil)
      case h :: t => if (h._1 == key) (true, t) else {
        val (found, newt) = removeMatch(t)
        (found, h :: newt)
      }
    }

    val index = hashFunc(key, table.length)
    val (found, lst) = removeMatch(table(index))
    table(index) = lst
    if (found) numElems -= 1
    this
  }

  def get(key: K): Option[V] = {
    val index = hashFunc(key, table.length)
    table(index).find(_._1 == key).map(_._2)
  }

  def iterator = new Iterator[(K, V)] {
    private var i1 = table.iterator
    private var i2 = nextBin()
    private def nextBin(): Iterator[(K, V)] = {
      if (i1.isEmpty) Iterator.empty
      else {
        var lst = i1.next
        while (lst.isEmpty && i1.hasNext) {
          lst = i1.next
        }
        lst.iterator
      }
    }

    def hasNext: Boolean = i2.hasNext

    def next: (K, V) = {
      val ret = i2.next
      if (i2.isEmpty) i2 = nextBin()
      ret
    }
  }

  override def isEmpty = numElems == 0

  override def size = numElems
  
  def maxBin = table.foldLeft(0)((m, b) => m max b.length)

  private def growTable(): Unit = {
    val tmp = Array.fill(table.length * 2 + 1)(List[(K, V)]())
    for (kv <- iterator) {
      val index = hashFunc(kv._1, tmp.length)
      tmp(index) ::= kv
    }
    table = tmp
  }
}

object ChainingHashMap {
  def apply[K, V](method: HashingMethods.Value, kvs: (K, V)*): ChainingHashMap[K, V] = {
    val ret = new ChainingHashMap[K, V](method)
    for (kv <- kvs) ret += kv
    ret
  }

  val fillingFactor = 0.7
}