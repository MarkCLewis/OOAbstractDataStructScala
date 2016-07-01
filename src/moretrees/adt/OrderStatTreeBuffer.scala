package moretrees.adt

import collection.mutable

class OrderStatTreeBuffer[A] extends mutable.Buffer[A] {
  abstract private class Node {
    def size: Int
    def height: Int
    def +(elem: A): Node
    def +:(elem: A): Node
    def apply(n: Int): A
    def remove(n: Int): (A, Node)
    def removeMin: (A, Node)
    def update(n: Int, elem: A): Unit
    def insert(n: Int, elem: A): Node
    def rotate: Node
  }

  private class INode(var data: A, var left: Node, var right: Node) extends Node {
    var size = 1 + left.size + right.size
    var height = 1 + (left.height max right.height)

    private def setSizeAndHeight: Unit = {
      size = 1 + left.size + right.size
      height = 1 + (left.height max right.height)
    }

    def +(elem: A): Node = {
      right += elem
      setSizeAndHeight
      rotate
    }

    def +:(elem: A): Node = {
      left += elem
      setSizeAndHeight
      rotate
    }

    def apply(n: Int): A = {
      if (n == left.size) data
      else if (n < left.size) left(n)
      else right(n - left.size - 1)
    }

    def remove(n: Int): (A, Node) = {
      val ret = if (left.size == n) {
        val d = data
        if (right == Empty) (data, left)
        else if (left == Empty) (data, right)
        else {
          val (newData, r) = right.removeMin
          right = r
          data = newData
          setSizeAndHeight
          (d, this)
        }
      } else if (left.size > n) {
        val (d, l) = left.remove(n)
        left = l
        setSizeAndHeight
        (d, this)
      } else {
        val (d, r) = right.remove(n - left.size - 1)
        right = r
        setSizeAndHeight
        (d, this)
      }
      (ret._1, ret._2.rotate)
    }

    def removeMin: (A, Node) = if (left == Empty) (data, right) else {
      val (d, l) = left.removeMin
      left = l
      setSizeAndHeight
      (d, rotate)
    }

    def update(n: Int, elem: A): Unit = {
      if (n == left.size) data = elem
      else if (n < left.size) left(n) = elem
      else right(n - left.size - 1) = elem
    }

    def insert(n: Int, elem: A): Node = {
      if (n <= left.size) left = left.insert(n, elem)
      else right = right.insert(n - left.size - 1, elem)
      setSizeAndHeight
      rotate
    }

    def rotate: Node = {
      if (left.height > right.height + 1) {
        left match {
          case lNode: INode => // Always works because of height.
            if (lNode.left.height > lNode.right.height) {
              rotateRight
            } else {
              left = lNode.rotateLeft
              rotateRight
            }
          case _ => null // Can't happen.
        }
      } else if (right.height > left.height + 1) {
        right match {
          case rNode: INode => // Always works because of height.
            if (rNode.right.height > rNode.left.height) {
              rotateLeft
            } else {
              right = rNode.rotateRight
              rotateLeft
            }
          case _ => null // Can't happen.
        }
      } else this
    }

    private def rotateLeft: INode = right match {
      case rNode: INode =>
        right = rNode.left
        rNode.left = this
        setSizeAndHeight
        rNode.setSizeAndHeight
        rNode
      case _ => throw new IllegalArgumentException("Rotate left called on node with empty right.")
    }

    private def rotateRight: INode = left match {
      case lNode: INode =>
        left = lNode.right
        lNode.right = this
        setSizeAndHeight
        lNode.setSizeAndHeight
        lNode
      case _ => throw new IllegalArgumentException("Rotate right called on node with empty left.")
    }
  }

  private object Empty extends Node {
    val size = 0
    val height = 0
    def +(elem: A): Node = new INode(elem, Empty, Empty)
    def +:(elem: A): Node = new INode(elem, Empty, Empty)
    def apply(n: Int): A = throw new IllegalArgumentException("Called apply on Empty.")
    def remove(n: Int): (A, Node) = throw new IllegalArgumentException("Called remove on Empty.")
    def removeMin: (A, Node) = throw new IllegalArgumentException("Called removeMin on Empty.")
    def update(n: Int, elem: A) = throw new IllegalArgumentException("Called update on Empty.")
    def insert(n: Int, elem: A): Node = new INode(elem, Empty, Empty)
    def rotate: Node = this
  }

  private var root: Node = Empty

  def +=(elem: A) = {
    root = root + elem
    this
  }

  def +=:(elem: A) = {
    root = elem +: root
    this
  }

  def apply(n: Int): A = {
    if (n >= root.size) throw new IndexOutOfBoundsException("Requested index "+n+" of "+root.size)
    root(n)
  }

  def clear(): Unit = {
    root = Empty
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    var m = 0
    for (e <- elems) {
      root = root.insert(n + m, e)
      m += 1
    }
  }

  def iterator = new Iterator[A] {
    var stack = mutable.Stack[INode]()
    def pushRunLeft(n: Node): Unit = n match {
      case in: INode =>
        stack.push(in)
        pushRunLeft(in.left)
      case Empty =>
    }
    pushRunLeft(root)
    def hasNext: Boolean = stack.nonEmpty
    def next: A = {
      val n = stack.pop
      pushRunLeft(n.right)
      n.data
    }
  }

  def length: Int = root.size

  def remove(n: Int): A = {
    if (n >= root.size) throw new IndexOutOfBoundsException("Remove index "+n+" of "+root.size)
    val (ret, node) = root.remove(n)
    root = node
    ret
  }

  def update(n: Int, newelem: A): Unit = {
    if (n >= root.size) throw new IndexOutOfBoundsException("Update index "+n+" of "+root.size)
    root(n) = newelem
  }
}