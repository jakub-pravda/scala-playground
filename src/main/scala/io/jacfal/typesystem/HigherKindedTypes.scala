package io.jacfal.typesystem

object HigherKindedTypes extends App {
  trait AHigherKindedType[F[_]]

  trait MyList[T] {
    def fatMap[B](f: T => B): MyList[B]
  }

  trait MyOption[T] {
    def fatMap[B](f: T => B): MyOption[B]
  }

  // combine/multiply List(1,2) x List("a", "b") => List(1a, 1b, 2a, 2b)

  def multiply[A, B](listA: List[A], listB: List[B]): List[(A, B)] = {
    for {
      a <- listA
      b <- listB
    } yield (a, b)
  }

  def multiply[A, B](listA: Option[A], listB: Option[B]): Option[(A, B)] = {
    for {
      a <- listA
      b <- listB
    } yield (a, b)
  }
  // how to generalize multiply?

  // use higher kinded type
  trait Monad[F[_], A] { // higher kinded type class
    def flatMap[B](f: A => F[B]): F[B] = ???
    def map[B](f: A => B): F[B] = ???
  }

  implicit class MonadList[A](l: List[A]) extends Monad[List, A] {
    override def flatMap[B](f: A => List[B]): List[B] = l.flatMap(f)
    override def map[B](f: A => B): List[B] = l.map(f)
  }

  def multiply[F[_], A, B](implicit ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  val monadList = new MonadList(List(1, 2, 3))
  monadList.flatMap(x => List(x, x + 1))
  // MonadPattern[List, Int] => List[Int]
  monadList.map(_ * 2)
  // MonadPattern[List, Int] => List[Int]

  multiply(List(1, 2), List("a", "b")) // it's compiled because of implicits
}
