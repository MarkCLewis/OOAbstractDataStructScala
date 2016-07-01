package moretrees.adt

import linkedlist.adt.ListStack

abstract sealed class ImmutableAVLTreeMap[K <% Ordered[K], +V] extends Map[K, V] {
  def +[B >: V](kv: (K, B)): ImmutableAVLTreeMap[K, B]
  def -(k: K): ImmutableAVLTreeMap[K, V]
  def rotate: ImmutableAVLTreeMap[K, V]
  val height: Int
  def verifyBalance: Boolean
}

private final class AVLNode[K <% Ordered[K], +V](
    private val key: K,
    private val data: V, private val left: ImmutableAVLTreeMap[K, V],
    private val right: ImmutableAVLTreeMap[K, V]) extends ImmutableAVLTreeMap[K, V] {

  val height = (left.height max right.height) + 1

  def +[B >: V](kv: (K, B)) = {
    val ret = if (kv._1 == key) new AVLNode(kv._1, kv._2, left, right)
    else if (kv._1 < key) new AVLNode(key, data, left + kv, right)
    else new AVLNode(key, data, left, right + kv)
    ret.rotate
  }

  def -(k: K) = {
    val ret = if (k == key) {
      left match {
        case _: AVLEmpty[K] => right
        case i: AVLNode[K, V] => {
          right match {
            case _: AVLEmpty[K] => left
            case _ => {
              val (k, d, newLeft) = i.removeMax
              new AVLNode(k, d, newLeft, right)
            }
          }
        }
      }
    } else if (k < key) {
      new AVLNode(key, data, left - k, right)
    } else {
      new AVLNode(key, data, left, right - k)
    }
    ret.rotate
  }

  def get(k: K): Option[V] = {
    if (k == key) Some(data)
    else if (k < key) left.get(k)
    else right.get(k)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = new ListStack[AVLNode[K, V]]
    pushRunLeft(AVLNode.this)
    def hasNext: Boolean = !stack.isEmpty
    def next: (K, V) = {
      val n = stack.pop()
      pushRunLeft(n.right)
      n.key -> n.data
    }
    def pushRunLeft(n: ImmutableAVLTreeMap[K, V]): Unit = {
      n match {
        case e: AVLEmpty[K] =>
        case i: AVLNode[K, V] =>
          stack.push(i)
          pushRunLeft(i.left)
      }
    }
  }

  def rotate: ImmutableAVLTreeMap[K, V] = {
    if (left.height > right.height + 1) {
      left match {
        case lNode: AVLNode[K, V] => // Always works because of height.
          if (lNode.left.height > lNode.right.height) {
            rotateRight
          } else {
            new AVLNode(key, data, lNode.rotateLeft, right).rotateRight
          }
        case _ => null // Can't happen.
      }
    } else if (right.height > left.height + 1) {
      right match {
        case rNode: AVLNode[K, V] => // Always works because of height.
          if (rNode.right.height > rNode.left.height) {
            rotateLeft
          } else {
            new AVLNode(key, data, left, rNode.rotateRight).rotateLeft
          }
        case _ => null // Can't happen.
      }
    } else this
  }

  def verifyBalance: Boolean = {
    left.verifyBalance && right.verifyBalance && (left.height - right.height).abs < 2
  }

  private def removeMax: (K, V, ImmutableAVLTreeMap[K, V]) = {
    right match {
      case e: AVLEmpty[K] => (key, data, left)
      case i: AVLNode[K, V] =>
        val (k, d, r) = i.removeMax
        (k, d, new AVLNode(key, data, left, r).rotate)
    }
  }

  private def rotateLeft: AVLNode[K, V] = right match {
    case rNode: AVLNode[K, V] =>
      new AVLNode(rNode.key, rNode.data, new AVLNode(key, data, left, rNode.left), rNode.right)
    case _ => throw new IllegalArgumentException("Rotate left called on node with empty right.")
  }

  private def rotateRight: AVLNode[K, V] = left match {
    case lNode: AVLNode[K, V] =>
      new AVLNode(lNode.key, lNode.data, lNode.left, new AVLNode(key, data, lNode.right, right))
    case _ => throw new IllegalArgumentException("Rotate right called on node with empty left.")
  }
}

private final class AVLEmpty[K <% Ordered[K]] extends ImmutableAVLTreeMap[K, Nothing] {
  def +[B](kv: (K, B)) = {
    new AVLNode(kv._1, kv._2, this, this)
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

  def rotate: ImmutableAVLTreeMap[K, Nothing] = this

  def verifyBalance: Boolean = true

  val height: Int = 0
}

object ImmutableAVLTreeMap {
  def apply[K <% Ordered[K], V](data: (K, V)*)(comp: (K, K) => Int): ImmutableAVLTreeMap[K, V] = {
    val empty = new AVLEmpty[K]()
    val d = data.sortWith((a, b) => comp(a._1, b._1) < 0)
    def binaryAdd(start: Int, end: Int): ImmutableAVLTreeMap[K, V] = {
      if (start < end) {
        val mid = (start + end) / 2
        new AVLNode(d(mid)._1, d(mid)._2, binaryAdd(start, mid), binaryAdd(mid + 1, end))
      } else empty
    }
    binaryAdd(0, data.length)
  }
}