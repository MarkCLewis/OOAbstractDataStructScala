package trees.adt

package trees.adt

import scala.annotation.tailrec
import scala.collection.mutable

class TreeMapRecur[K, V](comp: (K, K) => Int) extends mutable.Map[K, V] {
  private class Node(var key: K, var data: V) {
    var left: Node = null
    var right: Node = null
  }

  private var root: Node = null

  def +=(kv: (K, V)) = {
    def add(rover: Node): Node = {
      if (rover == null) {
        new Node(kv._1, kv._2)
      } else {
        var c = comp(kv._1, rover.key)
        if (c < 0) rover.left = add(rover.left)
        else if (c > 0) rover.right = add(rover.right)
        else {
          rover.key = kv._1
          rover.data = kv._2
        }
        rover
      }
    }
    root = add(root)
    this
  }

  def -=(key: K) = {
    def findVictim(n: Node): Node = {
      if (n == null) null
      else {
        val c = comp(key, n.key)
        if (c == 0) {
          if (n.left == null) n.right
          else if (n.right == null) n.left
          else {
            val (key, data, node) = deleteMaxChild(n.left)
            n.left = node
            n.key = key
            n.data = data
            n
          }
        } else if (c < 0) {
          n.left = findVictim(n.left)
          n
        } else {
          n.right = findVictim(n.right)
          n
        }
      }
    }

    def deleteMaxChild(n: Node): (K, V, Node) = {
      if (n.right == null) {
        (n.key, n.data, n.left)
      } else {
        val (key, data, node) = deleteMaxChild(n.right)
        n.right = node
        (key, data, n)
      }
    }

    root = findVictim(root)
    this
  }

  def get(key: K): Option[V] = {
    @tailrec def getter(rover: Node): Option[Node] = {
      if(rover==null) None
      else {
        var c = comp(key, rover.key)
        if(c==0) Some(rover)
        else if(c<0) getter(rover.left)
        else getter(rover.right)
      }
    }
    getter(root).map(_.data)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = new linkedlist.adt.ListStack[Node]
    pushRunLeft(root)
    def hasNext: Boolean = !stack.isEmpty
    def next: (K, V) = {
      val n = stack.pop()
      pushRunLeft(n.right)
      n.key -> n.data
    }
    @tailrec def pushRunLeft(n: Node) {
      if (n != null) {
        stack.push(n)
        pushRunLeft(n.left)
      }
    }
  }
}

object TreeMapRecur {
  def apply[K, V](data: (K, V)*)(comp: (K, K) => Int): TreeMap[K, V] = {
    val tm = new TreeMap[K, V](comp)
    val d = data.sortWith((a, b) => comp(a._1, b._1) < 0).toIndexedSeq
    def binaryAdd(start: Int, end: Int) {
      if (start < end) {
        val mid = (start + end) / 2
        tm += d(mid)
        binaryAdd(start, mid)
        binaryAdd(mid + 1, end)
      }
    }
    binaryAdd(0, data.length)
    tm
  }
}