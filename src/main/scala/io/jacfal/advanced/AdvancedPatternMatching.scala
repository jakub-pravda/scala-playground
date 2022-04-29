package io.jacfal.advanced

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"Only head $head is present")
    case _ =>
  }

  // What can be handled by pattern matching
  /*
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above
   */

  class Person(val name: String, val age: Int) // pure classes are compatible with pattern matching by default

  /* HOW TO DO IT? */
  object Person {
    def unapply(person: Person): Option[(String, Int)] = {
      if (person.age < 18)
        None
      else
        Some((person.name, person.age)) // Option[(String, Int)] -> name, age
    }

    def unapply(age: Int): Option[String] = Some(
      if (age < 18)
        "you can't"
      else
        "you can"
    )
  }

  val jacob = new Person("Jacob", 30)
  val john = new Person("John", 15)

  val greeting = john match {
    case Person(name, age) => s"My name is ${name} and I am $age years old"
    case _ => "Who are you?"
  }

  val allowed = jacob.age match {
    case Person(status) => s"Can I? => $status"
  }

  println(greeting)
  println(allowed)

  /*
    Exercise
   */

  class Number(val n: Int)
  object Number {
    def unapply(n: Int): Option[String] = Some(
      if (n < 10)
        "single digit"
      else if (n % 2 == 0)
        "Even number"
      else
        "Something else condition"
    )
  }

  val evenNum = new Number(21)
  println(evenNum.n match {
    case Number(x) => s"RESULT: $x"
  })

  // another more elegant solution
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10
  }

  println(9 match {
    case even() => "== EVEN NUMBER =="
    case singleDigit() => "== SINGLE DIGIT NUMBER =="
    case _ => "== NONE =="
  })

  // infix patterns
  case class Or[A, B](a: A, b: B) // infix patterns works only with two values at the pattern (something like Either)
  val e = Or(5, "five")

  println(e match {
    case num Or str => s"Number $num is $str"
  })

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => s"Starting with one"
  }

  abstract class MyList[+A] {
    def head: A = ???

    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1 & 2" // compile expects unapplySeq
    case _ => "Else..."
  }

  println(decomposed)

  // custom return types for unapply
  // Boolean or get (something)

  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false

      def get: String = person.name
    }
  }

  println(john match {
    case PersonWrapper(n) => s"My name is $n"
    case _ => s"Who am I?!"
  })
}
