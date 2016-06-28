package trees.adt

abstract sealed class ImmutableTreeMap[K <% Ordered[K], +V] extends Map[K, V] {
  def +[B >: V](kv: (K, B)): ImmutableTreeMap[K, B]
  def -(k: K): ImmutableTreeMap[K, V]
}

private class Node[K <% Ordered[K], +V](
    private val key: K,
    private val data: V,
    private val left: ImmutableTreeMap[K, V],
    private val right: ImmutableTreeMap[K, V]) extends ImmutableTreeMap[K, V] {

  def +[B >: V](kv: (K, B)) = {
    if (kv._1 == key) new Node(kv._1, kv._2, left, right)
    else if (kv._1 < key) new Node(key, data, left + kv, right)
    else new Node(key, data, left, right + kv)
  }

  def -(k: K) = {
    if (k == key) {
      left match {
        case _: Empty[K] => right
        case i: Node[K, V] => {
          right match {
            case _: Empty[K] => left
            case _ => {
              val (k, d, newLeft) = i.removeMax
              new Node(k, d, newLeft, right)
            }
          }
        }
      }
    } else if (k < key) {
      new Node(key, data, left - k, right)
    } else {
      new Node(key, data, left, right - k)
    }
  }

  def get(k: K): Option[V] = {
    if (k == key) Some(data)
    else if (k < key) left.get(k)
    else right.get(k)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = new linkedlist.adt.ListStack[Node[K, V]]
    pushRunLeft(Node.this)
    def hasNext: Boolean = !stack.isEmpty
    def next: (K, V) = {
      val n = stack.pop()
      pushRunLeft(n.right)
      n.key -> n.data
    }
    def pushRunLeft(n: ImmutableTreeMap[K, V]) {
      n match {
        case e: Empty[K] =>
        case i: Node[K, V] =>
          stack.push(i)
          pushRunLeft(i.left)
      }
    }
  }

  private def removeMax: (K, V, ImmutableTreeMap[K, V]) = {
    right match {
      case e: Empty[K] => (key, data, left)
      case i: Node[K, V] =>
        val (k, d, r) = i.removeMax
        (k, d, new Node(key, data, left, r))
    }
  }
}

private class Empty[K <% Ordered[K]] extends ImmutableTreeMap[K, Nothing] {
  def +[B](kv: (K, B)) = {
    new Node(kv._1, kv._2, this, this)
  }

  def -(k: K) = {
    this
  }

  def get(k: K): Option[Nothing] = {
    None
  }

  def iterator = new Iterator[(K, Nothing)] {
    def hasNext = false
    def next = null
  }
}

object ImmutableTreeMap {
  def apply[K <% Ordered[K], V](data: (K, V)*)(comp: (K, K) => Int): ImmutableTreeMap[K, V] = {
    val empty = new Empty[K]()
    val d = data.sortWith((a, b) => comp(a._1, b._1) < 0)
    def binaryAdd(start: Int, end: Int): ImmutableTreeMap[K, V] = {
      if (start < end) {
        val mid = (start + end) / 2
        new Node(d(mid)._1, d(mid)._2, binaryAdd(start, mid), binaryAdd(mid + 1, end))
      } else empty
    }
    binaryAdd(0, data.length)
  }
}