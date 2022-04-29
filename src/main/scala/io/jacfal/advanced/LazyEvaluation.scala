package io.jacfal.advanced

import scala.annotation.tailrec

object LazyEvaluation extends App {
  def sideEffectConditions: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectConditions
  println(if (lazyCondition && simpleCondition) "yes" else "no")

  def byNameMethod(n: => Int): Int = n + n + n + 1
  // CALL BY NEED
  def byNameMethodWithCallByNeed(n: => Int): Int = {
    lazy val t = n
    t + t + t + 1
  }

  def retrieveMagicValue(): Int = {
    // side effect or long computation
    Thread.sleep(1000)
    println("waiting")
    42
  }

  lazy val lazyRetrieveMagicValue: Int = {
    // side effect or long computation
    Thread.sleep(1000)
    println("waiting")
    42
  }

  println(byNameMethodWithCallByNeed(retrieveMagicValue()))

  // exercise: implement a lazily evaluated, singly linked stream of elements
}

// io.jacfal.edu.scalaadvancedcourse.MyStream.from(1)(x => x + 1) => stream of natural numbers (potentially infinite)
// naturals.take(100) // lazily evaluated stream of the first 100 naturals (finite stream)
// naturals.take(100).foreach(println) -> should print 100 naturals
// naturals.foreach(println) => should crash because stack overflow (infinite stream)
// naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A] // tail should be lazy?

  def #::[B >: A](element: B): MyStream[B] // prepend operator
  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concat two streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // take the firsts n elements out of this stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = // generator neni nic jineho nez dalsi funkce
    new NonEmptyMyStream[A](start, MyStream.from(generator(start))(generator)) // timhle vytvarim infinite stream
}

object EmptyMyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("head of empty stream")

  override def tail: MyStream[Nothing] = throw new UnsupportedOperationException("tail of empty list")

  override def #::[B >: Nothing](element: B): MyStream[B] = new NonEmptyMyStream[B](element, this)

  override def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = () // unit

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = List.empty
}

class NonEmptyMyStream[+A](h: A, t: => MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false

  override val head: A = h

  override lazy val tail: MyStream[A] = t // call by need

  override def #::[B >: A](element: B): MyStream[B] = new NonEmptyMyStream[B](element, this)

  override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = new NonEmptyMyStream[B](head, tail ++ anotherStream)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream[B] = new NonEmptyMyStream[B](f(head), tail.map(f))

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new NonEmptyMyStream[A](head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def take(n: Int): MyStream[A] = {
    if (n <= 0) EmptyMyStream
    else if (n == 1) new NonEmptyMyStream[A](head, EmptyMyStream)
    else new NonEmptyMyStream[A](head, tail.take(n - 1))
  }
}

object MyStreamTest extends App {
  val testStream = 1 #:: 2 #:: EmptyMyStream
  val testStream2 = 3 #:: 4 #:: EmptyMyStream

  val c =  testStream ++ testStream2
  println(c.tail.tail.head)

  val naturals = MyStream.from(0)(_ + 1)
  //println(naturals.map(_ * 2).take(10).toList())

  // Exercises
  // 1 - stream of Fibonacci numbers
  // 2 - stream of prime numbers with Era
}
