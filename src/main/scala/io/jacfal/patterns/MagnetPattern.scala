package io.jacfal.patterns

object MagnetPattern extends App {
  // example
  object ExternalLib {
    def giveMeResult(input: String): String = "twenty six"
    def giveMeResult(input: Int): Int = 26
    def giveMeResult(input: Boolean): Boolean = true
  }

  // what if giveMeResult are part of some lib and I need to extend them by another method?
  // answer is MAGNET pattern!

  object ExtendableExternalLib {
    trait GiveMeResultMagnet[Result] {
      def apply(): Result
    }
    def giveMeResult[R](magnet: GiveMeResultMagnet[R]): R = magnet()

    implicit class FromString(input: => String) extends GiveMeResultMagnet[String] {
      def apply(): String = "twenty six"
  
    }

    implicit class FromInt(input: => Int) extends GiveMeResultMagnet[Int] {
      def apply(): Int = 26
    }

    implicit class FromBoolean(input: => Boolean) extends GiveMeResultMagnet[Boolean] {
      def apply(): Boolean = true
    }
  }

  println(ExtendableExternalLib.giveMeResult("Hey"))

  // extends by another...
  implicit class FromLong(input: => Long) extends ExtendableExternalLib.GiveMeResultMagnet[Long] {
    def apply(): Long = 30L
  }
  println(ExtendableExternalLib.giveMeResult(30L))
}
