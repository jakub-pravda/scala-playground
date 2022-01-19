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
  def ++[S >: T](anotherList: MyList[S]): MyList[S]
  def removeAt(index: Int): MyList[T]
}

object MyList {
  def from[T](iter: Iterable[T]): MyList[T] = {
    @tailrec
    def tooMyListRecursive(remaining: Iterable[T], acc: MyList[T]): MyList[T] = {
      if (remaining.isEmpty) acc
      else tooMyListRecursive(remaining.tail, remaining.head :: acc)
    }
    tooMyListRecursive(iter, MyNil).reverse
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

  override def ++[S >: Nothing](anotherList: MyList[S]): MyList[S] = anotherList

  override def removeAt(index: Int): MyList[Nothing] = MyNil
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

  override def ++[S >: T](anotherList: MyList[S]): MyList[S] = {
    @tailrec // complexity: 2*O(N)
    def addRecursive(remaining: MyList[S], acc: MyList[S]): MyList[S] = {
      if (remaining.isEmpty) acc
      else addRecursive(remaining.tail, remaining.head :: acc)
    }
    addRecursive(this.reverse, anotherList)
  }

  override def removeAt(index: Int): MyList[T] = {
    @tailrec // complexity: O(N)
    def removeAtRecursive(remaining: MyList[T], acc: MyList[T], currentIndex: Int): MyList[T] = {
      if(remaining.isEmpty) this // index out of bounds
      else if (currentIndex == index) acc.reverse ++ remaining.tail
      else removeAtRecursive(remaining.tail, remaining.head :: acc, currentIndex + 1)
    }

    if (index < 0) this
    else removeAtRecursive(this, MyNil, 0)
  }
}

object ListsProblems extends App {
  val testList1 =  1 :: 2 :: 3 :: 4 :: MyNil
  val testList2 =  5 :: 6 :: 7 :: 8 :: MyNil
  val largeList = MyList.from(1 to 1000)
  val shortList = 1 :: MyNil
  val emptyList = MyNil

  // search the k-th element
  assert(testList1(0) == 1)
  assert(testList1(3) == 4)
  assert(Try(testList1(-1)).isFailure)
  assert(Try(testList1(10)).isFailure)
  println("Search k-th element tests OK")

  // list length test
  assert(testList1.length == 4)
  assert(shortList.length == 1)
  assert(emptyList.length == 0)
  println("Length tests OK")

  // reverse list
  assert(testList1.reverse.toString == "[4, 3, 2, 1]")
  assert(shortList.reverse.toString == "[1]")
  assert(emptyList.reverse == MyNil)
  println("Reverse tests OK")

  // list concatenate
  assert((testList1 ++ testList2).toString == "[1, 2, 3, 4, 5, 6, 7, 8]")
  assert((emptyList ++ testList1).toString == "[1, 2, 3, 4]")
  assert((testList1 ++ emptyList).toString == "[1, 2, 3, 4]")
  println("List concat tests OK")

  // remove k-th element
  assert(testList1.removeAt(1).toString == "[1, 3, 4]")
  assert(testList1.removeAt(3).toString == "[1, 2, 3]")
  assert(testList1.removeAt(10).toString == "[1, 2, 3, 4]")
  assert(shortList.removeAt(0) == MyNil)
  println("Remove k-th element test OK")
}
