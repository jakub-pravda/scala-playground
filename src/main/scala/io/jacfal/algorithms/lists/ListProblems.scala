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
  def map[S](f: T => S): MyList[S]
  def flatMap[S](f: T => MyList[S]): MyList[S]
  def filter(f: T => Boolean): MyList[T]

  def getIndex[S >: T](value: S): Int
  def runLengthEncoding: MyList[(T, Int)]
  def duplicateEachElement(k: Int): MyList[T]
  def rotate(k: Int): MyList[T]
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

  override def map[S](f: Nothing => S): MyList[S] = MyNil

  override def flatMap[S](f: Nothing => MyList[S]): MyList[S] = MyNil

  override def filter(f: Nothing => Boolean): MyList[Nothing] = MyNil

  override def getIndex[S >: Nothing](value: S): Int = -1

  override def runLengthEncoding: MyList[(Nothing, Int)] = MyNil

  override def duplicateEachElement(k: Int): MyList[Nothing] = MyNil

  override def rotate(k: Int): MyList[Nothing] = MyNil
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

  override def map[S](f: T => S): MyList[S] = {
    @tailrec // complexity: O(N)
    def mapRecursive(remaining: MyList[T], acc: MyList[S]): MyList[S] = {
      if (remaining.isEmpty) acc
      else mapRecursive(remaining.tail, f(remaining.head) :: acc)
    }
    mapRecursive(this, MyNil).reverse
  }

  override def flatMap[S](f: T => MyList[S]): MyList[S] = {
    @tailrec
    def flatMapRecursive(remaining: MyList[T], acc: MyList[S]): MyList[S] = {
      if (remaining.isEmpty) acc
      else flatMapRecursive(remaining.tail,  acc ++ f(remaining.head))
    }
    flatMapRecursive(this, MyNil)
  }

  override def filter(f: T => Boolean): MyList[T] = {
    @tailrec
    def filterRecursive(remaining: MyList[T], acc: MyList[T]): MyList[T] = {
      if (remaining.isEmpty) acc
      else {
        val newAcc = {
          if (f(remaining.head)) remaining.head :: acc
          else acc
        }
        filterRecursive(remaining.tail, newAcc)
      }
    }
    filterRecursive(this, MyNil).reverse
  }

  override def runLengthEncoding: MyList[(T, Int)] = {
    @tailrec
    def runLengthEncodingRecursive(remaining: MyList[T], acc: MyList[(T, Int)]): MyList[(T, Int)] = {
      if (remaining.isEmpty) acc
      else {
        val keys = acc.map(_._1)
        val headIndex = keys.getIndex(remaining.head)
        val newAcc = {
          if (headIndex < 0) (remaining.head, 1) :: acc
          else {
            val newItem = (remaining.head, acc(headIndex)._2 + 1)
            val accWithoutHead = acc.removeAt(headIndex)
            newItem :: accWithoutHead
          }
        }
        runLengthEncodingRecursive(remaining.tail, newAcc)
      }
    }
    runLengthEncodingRecursive(this, MyNil)
  }

  override def getIndex[S >: T](value: S): Int = {
    @tailrec
    def getIndexRecursive(remaining: MyList[S], currentIndex: Int): Int = {
      if (remaining.isEmpty) -1
      else if (remaining.head == value) currentIndex
      else getIndexRecursive(remaining.tail, currentIndex + 1)
    }
    getIndexRecursive(this, 0)
  }

  override def duplicateEachElement(k: Int): MyList[T] = {
    // complexity O(N * K)
    @tailrec
    def duplicateRecursive(remaining: MyList[T], acc: MyList[T], repeat: Int): MyList[T] = {
      if (remaining.isEmpty) acc
      else if (repeat > 0) duplicateRecursive(remaining, remaining.head :: acc, repeat - 1)
      else duplicateRecursive(remaining.tail, acc, k)
    }

    if (k <= 0) this
    else duplicateRecursive(this, MyNil, k).reverse
  }

  override def rotate(k: Int): MyList[T] = {
    val rotateIndex = k % this.length

    @tailrec
    def rotateRecursive(remaining: MyList[T], currentPosition: Int, acc: MyList[T]): MyList[T] = {
      if (currentPosition > rotateIndex) remaining ++ acc.reverse
      else rotateRecursive(remaining.tail, currentPosition + 1, remaining.head :: acc)
    }

    if (rotateIndex <= 0) this
    rotateRecursive(this, 1, MyNil)
  }
}

object ListsProblems extends App {
  val testList1 =  1 :: 2 :: 3 :: 4 :: MyNil
  val testList2 =  5 :: 6 :: 7 :: 8 :: MyNil
  val largeList = MyList.from(1 to 1000)
  val shortList = 1 :: MyNil
  val emptyList = MyNil
  val runLengthEncodingList = 1 :: 3 :: 2 :: 1 :: 3 :: 1 :: MyNil

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
  println("Remove k-th element tests OK")

  // map
  assert(testList1.map(x => x * 10).toString == "[10, 20, 30, 40]")
  assert(testList2.map(x => x + 10).toString == "[15, 16, 17, 18]")
  assert(shortList.map(x => x + 1).toString == "[2]")
  assert(emptyList.map(x => x) == MyNil)
  println("List map tests OK")

  // flatmap
  assert(testList1.flatMap(n => n + 1 :: MyNil).toString == "[2, 3, 4, 5]")
  assert(shortList.flatMap(n => n * 10 :: MyNil).toString == "[10]")
  assert(testList1.flatMap(x => x :: (x * 2) :: MyNil).toString == "[1, 2, 2, 4, 3, 6, 4, 8]")
  println("Flatmap tests OK")

  // filter
  assert(largeList.filter(n => n < 5).toString == "[1, 2, 3, 4]")
  assert(largeList.filter(n => n < 0) == MyNil)
  println("Filter tests OK")

  // run length encoding
  assert(testList1.runLengthEncoding.toString == "[(4,1), (3,1), (2,1), (1,1)]")
  assert(runLengthEncodingList.runLengthEncoding.toString == "[(1,3), (3,2), (2,1)]")
  println("Run length encoding tests OK")

  // duplicate each element tests
  assert(testList1.duplicateEachElement(3).toString == "[1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]")
  assert(testList2.duplicateEachElement(1).toString == "[5, 6, 7, 8]")
  assert(testList2.duplicateEachElement(-1).toString == "[5, 6, 7, 8]")
  println("Duplicate each element tests OK")

  assert(testList1.rotate(3).toString == "[4, 1, 2, 3]")
  assert(testList1.rotate(1).toString == "[2, 3, 4, 1]")
  assert(testList2.rotate(5).toString == "[6, 7, 8, 5]")
  println("Rotate tests OK")
}
