package io.jacfal.algorithms.lists

import scala.annotation.tailrec

sealed abstract class MyList[+T] {
  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): MyList[S] = new ::[S](elem, this) // right associative
}

case object MyNil extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // side effect (bad habit)
  override def tail: MyList[Nothing] = throw new NoSuchElementException // side effect (bad habit)
  override def isEmpty: Boolean = true

  override def toString: String = "[]"
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
}

object ListsProblems extends App {
  val testList =  1 :: 2 :: 3 :: MyNil
  println(testList.toString)
}
