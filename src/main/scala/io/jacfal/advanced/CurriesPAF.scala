package io.jacfal.advanced

object CurriesPAF extends App {
  // curried functions
  val superAdder: Int => Int => Int = {
    x => y => x + y // curried function, return another function
  }

  val test = superAdder(3)
  val test2 = test(1)

  def curriedMethod(x: Int)(y: Int): Int = x + y

  val curr: Int => Int = curriedMethod(10) // must be defined as function or compiler err
  // lifting = ETA-EXPANSION
  // functions != methods (JVM limitations)

  def inc(x: Int): Int = x + 1

  Seq(1, 2, 3).map(inc /* x => inc(x) */) // inc is converted to the function ETA-expansion

  // Partial functions applications
  val add5 = curriedMethod(5) _ // Int => Int, _ tell's the compiler to use ETA-EXPANSION

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = simpleAddFunction(7, _)
  val add71 = simpleAddMethod(7, _: Int) // compiler shortcut y => simpleAddMethod(7, y)
  val add72 = curriedAddMethod(7) _
  //val add73 = curriedAddMethod _ (7) // partially applied function
  val add74 = simpleAddFunction(simpleAddMethod(3, curriedAddMethod(1)(1)), _)
  val add75 = (x: Int) => simpleAddMethod(7, x)
  val add76: Int => Int = (x: Int) => simpleAddMethod(7, x) // shortcut above

  // Another exercise
  // 1.
  val testData = Seq(1.45, 4.32, 6.16789, 4.567)
  val formatNumbers = (nums: Seq[Double], formatter: String) => nums.foreach(n => println(formatter.format(n)))
  val formatter1 = formatNumbers(_: Seq[Double], "%4.2f")
  val formatter2 = formatNumbers(_: Seq[Double], "%8.6f")
  val formatter3 = formatNumbers(_: Seq[Double], "%14.12f")

  println("Exercise 1")
  formatter1(testData)

  // TODO exercise number 2
}
