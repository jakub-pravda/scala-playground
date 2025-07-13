package io.jacfal.effects

/*  
  Effects desire:
  - should describe what kind of computation will perform
  - should describe the VALUE that effect will produce
  - **if side effect required**, then construction of the data structure must be separated from the execution of effect
*/

// Referential transparency:
// * deterministic output
// * no side effects
// * substitutability

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
  // 1. measure the current time of the system
  val timeMeasurement = MyIO[Long](() => System.currentTimeMillis()) // time effect
  println(s"Current millis time result: ${timeMeasurement.unsafeRun()}")
  
  // 2. measure duration of a computation
  //   - use map/flatmap combination of IO
  def measurement[A](computataion: MyIO[A]): MyIO[(Long, A)] = for {
    start <- timeMeasurement // run time effect to get time
    result <- computataion
    end <- timeMeasurement // run time effect to get time
  } yield (end - start, result)

  val computataion = MyIO[Boolean](() => {
    Thread.sleep(1000)
    true
  })
  val resultOfMeasurement = measurement(computataion)
  println(s"Time measurement result: ${resultOfMeasurement.unsafeRun()}")
  
  // 3. read something from the console
  // we need side effect to read user input from the console
  import scala.io.StdIn.readLine
  
  val readFromConsoleEffect = MyIO[String](() => {
    readLine()
  })
  
  // 4. print something to the console, then read, then print a welcome message
  val printHello = MyIO[Unit](() => println("Hello, world! Please insert your name:"))
  def printName(name: String) = MyIO[Unit](() => println(s"Hello $name"))
    
  def writeReadConsoleEffect(): MyIO[Unit] = for {
    _     <- printHello
    input <- readFromConsoleEffect
    _     <- printName(input)
  } yield ()
  
  writeReadConsoleEffect().unsafeRun()
}
