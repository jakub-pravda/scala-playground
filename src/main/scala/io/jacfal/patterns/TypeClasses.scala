package io.jacfal.patterns

/*
* Some notes:
* Polymorphism - the same operation working on different types of value
* typeclasses = decoupled ad hoc polymorphism
*/

case class User(name: String, age: Int)

// TYPE CLASS
trait Equal[T] { // Type class
  def apply(a: T, b: T): Boolean
}

object Equal {
  def compare[T](a: T, b: T)(implicit equal: Equal[T]): Boolean = equal(a, b)
  def apply[T](implicit equal: Equal[T]): Equal[T] = equal // OPTIONAL access to the entire type class interface
}

object TypeClasses extends App {
  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name // Type class instances
  }

  implicit object IntEquality extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b

  }

  val user1 = User("Jacob", 30)
  val user2 = User("Jacob", 30)

  println(Equal.compare(user1, user2))
  println(Equal.compare(1, 2))
}
