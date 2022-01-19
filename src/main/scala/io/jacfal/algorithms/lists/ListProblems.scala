package io.jacfal.algorithms.lists

import scala.annotation.tailrec
import scala.util.Try

sealed abstract class MyList[+T] {
  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): MyList[S] = new ::[S](elem, this) // right associative

  def apply(index: Int): T
  def length: Int
  def reverse: MyList[T]
}

object MyList {
  def from[T](iter: Iterable[T]): MyList[T] = {
    ???
  }
}

case object MyNil extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // side effect (bad habit)
  override def tail: MyList[Nothing] = throw new NoSuchElementException // side effect (bad habit)
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: MyList[Nothing] = MyNil
}

case class ::[+T](override val head: T, override val tail: MyList[T]) extends MyList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: MyList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) toStringTailRec(remaining.tail, s"$result${remaining.head}")
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }
    "[" + toStringTailRec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    @tailrec // O(min(N, index))
    def applyRec(remaining: MyList[T], currentIndex: Int) : T = {
      if (currentIndex == index) remaining.head
      else applyRec(remaining.tail, currentIndex + 1)
    }
    if (index < 0) throw new IndexOutOfBoundsException

    applyRec(this, 0)
  }

  override def length: Int = {
    @tailrec // O(N)
    def lengthRec(remaining: MyList[T], accumulator: Int): Int = {
      if(remaining.isEmpty) accumulator
      else lengthRec(remaining.tail, accumulator + 1)
    }

    lengthRec(this, 0)
  }

  override def reverse: MyList[T] = {
    @tailrec // complexity O(N)
    def reverseRec(remaining: MyList[T], accumulator: MyList[T]): MyList[T] = {
      if (remaining.isEmpty) accumulator
      else reverseRec(remaining.tail, remaining.head :: accumulator)
    }

    reverseRec(this, MyNil)
  }
}

object ListsProblems extends App {
  val testList =  1 :: 2 :: 3 :: 4 :: MyNil
  val shortList = 1 :: MyNil
  val emptyList = MyNil

  // search the k-th element
  assert(testList(0) == 1)
  assert(testList(3) == 4)
  assert(Try(testList(-1)).isFailure)
  assert(Try(testList(10)).isFailure)
  println("Search k-th element tests OK")

  // list length test
  assert(testList.length == 4)
  assert(shortList.length == 1)
  assert(emptyList.length == 0)
  println("Length tests OK")

  // reverse list
  assert(testList.reverse.toString == "[4, 3, 2, 1]")
  assert(shortList.reverse.toString == "[1]")
  assert(Try(emptyList.reverse.toString).isFailure)
  println("Reverse tests OK")
}
