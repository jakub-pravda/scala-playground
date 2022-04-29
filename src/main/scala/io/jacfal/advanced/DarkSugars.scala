package io.jacfal.advanced

object DarkSugars extends App {
  // #1 methods with single param
  def singleArgMethod(arg: Int) = arg + 10

  val des = singleArgMethod {
    val num = 1
    val num2 = 3

    num + num2
  }

  List(1, 2, 3).map {
    x => x + 1
  }

  // #2 single abstract method pattern
  trait Action {
    def act(x: Int): Int
  }

  val instance: Action = new Action {
    override def act(x: Int): Int = x + 1
  } // this expression could be rewritten by single abstract method pattern

  val AnotherInstance: Action = (x: Int) => x + 1 // using lambda

  abstract class AnAbstractType {
    def implemented: Int = 23

    def f(a: Int): Unit
  }

  // unit definition with lambda in anonymous class
  val abstractInstance: AnAbstractType = (x: Int) => println("Cool") // no implicits, just a syntax sugar!
  abstractInstance.f(2)

  println("END")

  // #3 :: and #:: methods are special
  val l1 = 2 :: List(3, 4) // compiler rewrites these expression as...
  List(3, 4).::(2)

  // scala spec: last char decides associativity of method
  1 :: 2 :: 3 :: List(4, 5) // is compiled as...
  List(4, 5).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // right associative because it ends with : (same as the List)
  }

  val myStream = 1 -->: 2 -->: new MyStream[Int]
  new MyStream[Int].-->:(2).-->:(1) // same as upper

  // #4 multi word method naming (language feature)
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"Some gossip ${gossip}")
  }

  val alice = new TeenGirl("Alice")
  alice `and then said` "cool"

  // # 5 infix types
  class Composite[A, B]
  val composite: Composite[String, Int] = ??? // classic mode
  val compositeInfix: Int Composite String = ???

  class -->[A, B]
  val arrow: String --> Int = ???

  // # 6 update method
  val anArray = Array(1, 2, 3)
  anArray(2) = 4 // rewritten to...
  anArray.update(2, 4)
  // very used in mutable collections (update() is much like apply())

  // # 7 setters mutable container (C# properties simulation)
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation

    def member: Int = internalMember // getter

    def member_=(value: Int): Unit = internalMember = value // setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 45 // rewritten as...
  aMutableContainer.member_=(45)

}
