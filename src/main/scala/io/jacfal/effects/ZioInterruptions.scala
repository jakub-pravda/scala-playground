//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package io.jacfal.effects

import zio._

// run me: scala-cli -S 2.13.16 src/main/scala/io/jacfal/effects/Utils.scala src/main/scala/io/jacfal/effects/ZioInterruptions.scala
object Interruptions extends ZIOAppDefault {
  import io.jacfal.effects.Utils._
  
  val zioWithTime = ZIO.succeed("starting computitation").debugThread *> 
    ZIO.sleep(2.seconds) *>
    ZIO.succeed(42).debugThread
  
  val interruption = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interrupt /* is an effect, this blocks calling fiber */
    _ <- ZIO.succeed("Interruption succeed").debugThread
    result <- fib.join
  } yield result
  
  // How to interrupt without blocking?
  val interruption_v2 = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interruptFork
    _ <- ZIO.succeed("Interruption succeed").debugThread
    result <- fib.join
  } yield result
  
  // def run = interruption
  def run = interruption_v2
}
