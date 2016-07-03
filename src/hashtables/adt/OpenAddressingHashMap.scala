package hashtables.adt

import collection.mutable

class OpenAddressingHashMap[K, V](
    hashMethod: HashingMethods.Value,
    probeMethod: ProbingMethods.Value) extends mutable.Map[K, V] {
  import OpenAddressingHashMap._
  private var numElems = 0
  private var numEntriesUsed = 0
  private var table = Array.fill(11)(Empty: Entry[K, V])
  private val hashFunc = HashingMethods.buildHashFunction(hashMethod)
  private val probeFunc = ProbingMethods.buildProbingFunction(probeMethod)

  def +=(kv: (K, V)): OpenAddressingHashMap.this.type = {
    if (numEntriesUsed > OpenAddressingHashMap.fillingFactor * table.length) growTable()
    val index = hashFunc(kv._1, table.size)
    (0 until table.length).find { i =>
      val probeIndex = probeFunc(index, i, table.size)
      table(probeIndex) match {
        case Empty =>
          numElems += 1
          numEntriesUsed += 1
          table(probeIndex) = new Full(kv)
          true
        case Removed =>
          numElems += 1
          table(probeIndex) = new Full(kv)
          true
        case f: Full[K, V] if f.key == kv._1 =>
          table(probeIndex) = new Full(kv)
          true
        case _ => false
      }
    }
    this
  }

  def -=(key: K): OpenAddressingHashMap.this.type = {
    val index = hashFunc(key, table.size)
    (0 until table.length).find { i =>
      val probeIndex = probeFunc(index, i, table.size)
      table(probeIndex).key == Some(key) || table(probeIndex) == Empty
    }.foreach { i =>
      val probeIndex = probeFunc(index, i, table.size)
      if(table(probeIndex) != Empty) {
        table(probeIndex) = Removed
        numElems -= 1
      }
    }
    this
  }

  def get(key: K): Option[V] = {
    val index = hashFunc(key, table.size)
    ((0 until table.length).find { i =>
      val probeIndex = probeFunc(index, i, table.size)
      table(probeIndex) == Empty || table(probeIndex).key == Some(key)
    }).flatMap(i => table(probeFunc(index, i, table.size)).value)
  }

  def iterator = new Iterator[(K, V)] {
    def advance() = while (index < table.length && table(index).key == None) 
        index += 1
    var index = 0
    advance()

    def hasNext: Boolean = index < table.length
    def next: (K, V) = {
      val ret = table(index).kv.get
      index += 1
      advance()
      ret
    }
  }

  override def isEmpty = numElems == 0

  override def size = numElems

  private def growTable(): Unit = {
    val tmp = Array.fill(table.length * 2 + 1)(Empty: Entry[K, V])
    for (kv <- iterator) {
      val index = hashFunc(kv._1, tmp.length)
      var i = 0
      var probeIndex = probeFunc(index, i, tmp.length)
      while (tmp(probeIndex) != Empty) {
        i += 1
        probeIndex = probeFunc(index, i, tmp.length)
      }
      tmp(probeIndex) = new Full(kv)
    }
    table = tmp
    numEntriesUsed = numElems
  }
}

object OpenAddressingHashMap {
  def apply[K, V](hashMethod: HashingMethods.Value, probeMethod: ProbingMethods.Value, kvs: (K, V)*): OpenAddressingHashMap[K, V] = {
    val ret = new OpenAddressingHashMap[K, V](hashMethod, probeMethod)
    for (kv <- kvs) ret += kv
    ret
  }

  private val fillingFactor = 0.5

  private sealed trait Entry[+K, +V] {
    val key: Option[K]
    val value: Option[V]
    val kv: Option[(K, V)]
  }
  private object Empty extends Entry[Nothing, Nothing] {
    val key = None
    val value = None
    val kv = None
  }
  private object Removed extends Entry[Nothing, Nothing] {
    val key = None
    val value = None
    val kv = None
  }
  private class Full[K, V](val _kv: (K, V)) extends Entry[K, V] {
    val key = Some(_kv._1)
    val value = Some(_kv._2)
    val kv = Some(_kv)
  }
}