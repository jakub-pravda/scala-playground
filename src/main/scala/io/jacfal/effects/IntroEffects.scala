package io.jacfal.effects

/*  
  Effects desire:
  - should describe what kind of computation will perform
  - should describe the VALUE that effect will produce
  - **if side effect required**, then construction of the data structure must be separated from the execution of effect
*/

case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] = 
    MyIO(() => f(unsafeRun()))
  
  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())
}

object IntroEffects extends App {
  // Functional programming
  // local reasoning
  //   - type signature describes the kind of computation that will be performed
  // referential transparency
  //   - ability to replace expression with the value that it evaluates to
  
  // referantial transparency is not allways achievable
  val printResult: Unit = println("Hello, World!") // print is side effect
  val printResult2: Unit = ()
  // expressions are not same! But both returns Unit...
  // but side effeccts are inevitable
  
  // Crate IO datatype 
  // 1. measure the current tyme of the system
  val timeMeasurement = MyIO[Long](() => System.currentTimeMillis())
  print(s"Time measurement result: ${timeMeasurement.unsafeRun()}")
  
  // 2. measure duration of a computation
  //   - use map/flatmap combination of IO
  // 3. read something from the console
  // 4. print something to the console, then read, then print a welcome message
}
