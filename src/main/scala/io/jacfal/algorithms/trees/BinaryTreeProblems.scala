package io.jacfal.algorithms.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
  val size: Int
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List.empty

  override def leafCount: Int = 0

  override val size: Int = 0
}

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collectLeavesRecursive(remaining: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
      if (remaining.isEmpty) acc
      else if (remaining.head.isEmpty) collectLeavesRecursive(remaining.tail, acc)
      else if (remaining.head.isLeaf) collectLeavesRecursive(remaining.tail, remaining.head +: acc)
      else collectLeavesRecursive(remaining.head.left :: remaining.head.right :: remaining.tail, acc)
    }

    collectLeavesRecursive(List(this), List.empty)
  }

  override def leafCount: Int = collectLeaves.size

  override val size: Int = 1 + right.size + left.size
}

object BinaryTreeProblems extends App {
  // ** TESTS **
  val leaf6 = BNode[Int](6, BEnd, BEnd)
  val node3 = BNode[Int](3, BEnd, leaf6)
  val leaf4 = BNode[Int](4, BEnd, BEnd)
  val leaf5 = BNode[Int](5, BEnd, BEnd)
  val node2 = BNode[Int](2, leaf4, leaf5)
  val node1 = BNode[Int](1, node2, node3)

  assert(node1.collectLeaves.map(_.value) == List(6, 5, 4))
  assert(node1.leafCount == 3)
  println("Collect leaves tests OK")

  assert(node1.size == 6)
  println("Size tests OK")
}
