//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package io.jacfal.effects

import zio._
import zio.ZIOAppDefault

object Fibers extends ZIOAppDefault {
  import io.jacfal.effects.Utils._
  
  val zioFirst = ZIO.succeed("Hello World")  
  val zioSecond = ZIO.succeed("Hello Czech Republic!")

  // Fiber = lightweight thread
  def createFiber: Fiber[Throwable, String] = ??? // just simple fibre definition
  // It's hard to create fibers manually, we can use ZIO api instead
  
  def fiberTest = for {
    // same thread IO - running sequentially
    hello <- zioFirst.debugThread
    czech <- zioSecond.debugThread
  } yield (hello, czech)
  
  def fiberParallelTest = for {
    // same thread IO - running in parallel
    fib1 <- zioFirst.debugThread.fork
    fib2 <- zioSecond.debugThread.fork
  } yield (fib1, fib2)
  
  def runOnThread[R,E,A](zio: ZIO[R,E,A]) = for {
    // join fibers to wait for their completion
    fib <- zio.debugThread.fork
    result <- fib.await // or use join to get the result directly 
  } yield result match {
    case Exit.Success(value) => s"Succeed with $value"
    case Exit.Failure(cause) => s"Error with $cause"
  }
  
  // remark: Fibers are very cheap
  //  simple datastructure stored in the heap
  //  when fibers are not referenced, they are garbage collected
  
  // ** Exercice ** 
  // TODO
  
  // How fibers works?
  
  def run = runOnThread(fiberTest).debugThread
}