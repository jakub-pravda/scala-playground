package io.jacfal.advanced

import scala.util.{Failure, Try}

object PartialFunctions extends App {
  val aFunction = (x: Int) => x + 10

  // what if I need some restrictions to input value? Eg. x >= 1 and x<=5, or I need return some specific value for
  //  certain input number? I can use if statements or pattern matching, but it is a little bit clumsy. Partial
  //  functions are better option

  // partial function definition
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 16
    case 2 => 32
    case 3 => 64
  }

  println(aPartialFunction(1))
  Try(aPartialFunction(54545)) match { // it ends with match err exception, because partial functions are based on the pattern matching
    case Failure(exception) => println(exception)
  }

  // partial functions utilities
  println(aPartialFunction.isDefinedAt(1)) // true
  println(aPartialFunction.isDefinedAt(10)) // false

  val lifted = aPartialFunction.lift // Int -> Option[int]
  println(lifted(2)) // Some(32)

  // you can chaining partial functions with orElse
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 4 => 128
  }
  println(pfChain(4)) // return 128

  // PF can extends normal functions => actually partial functions could act as require checkers
  // HOFs accepts partial functions as well

  val aFunctionsWithPartial = (x: Int) => {
    aPartialFunction.lift(x) match {
      case Some(v) => v
      case None => x + 10
    }
  }

  println(aFunctionsWithPartial(1)) // return 16
  println(aFunctionsWithPartial(10)) // return 20

  // !!! NOTE !!!
  // * PF can only have ONE parameter type

  // Exercises
  // 1 - construct a PF instance yourself (anonymous class)
  val anonPartial = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 16
      case 2 => 32
      case 1 => 64
    }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 3
  }

  // 2 - dumb chat bot as a PF
  println("\n\n=== CHAT BOT ===")

  val chatBotPartial: PartialFunction[String, String] = {
    case "hello" => "Hello, man!"
    case "how are you?" => "I am fine, thanks man"
    case "?" => "What do you need man?"
  }

  val chatBotFull = (phrase: String) => {
    chatBotPartial.lift(phrase) match {
      case Some(answer) => answer
      case None => "I don't know what are you talking about man..."
    }
  }

  scala.io.Source.stdin.getLines().foreach(line => println(chatBotFull(line.toLowerCase)))
}
