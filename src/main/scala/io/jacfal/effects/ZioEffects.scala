//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package jacfal.io.effects

import zio._
import scala.io.StdIn

// simplified raw ZIO
// R - Input type, E - Error type, A - Result type
case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
  
  def map[B](f: A => B): MyZIO[R, E, B] = {
    MyZIO(r => unsafeRun(r) match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    })
  }
  
  def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] = {
    MyZIO(r => unsafeRun(r) match {
      case Left(e) => Left(e)
      case Right(v) => f(v).unsafeRun(r)
    })
  }
}

object Main extends App {
  // Example how to construct ZIO effect
  val testZIO: ZIO[Any, Nothing, Int] = ZIO.succeed(26)
  
  // map + flatmap
  val printEffect = testZIO.map(_ * 2).flatMap(result => ZIO.succeed(println(result)))
  
  val smallProgram = for {
    _ <- ZIO.succeed(println("What's your name?"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Your name is $name"))
  } yield()
  
  // Most common ZIO type aliases
  // UIO[A] = ZIO[Any, Nothing, A]
  // Task[A] = ZIO[Any, Throwable, A]
  // IO[E, A] = ZIO[Any, E, A]
  
  // ** Some exercises **
  // Sequence two zio effects - return last
  def sequenceTakeLast[R,E,A,B](zioA: ZIO[R,E,A], zioB: ZIO[R,E,B]): ZIO[R,E,B] = for {
    _ <- zioA
    b <- zioB
  } yield (b) // same as zioA *> zioB
  
  // Sequence two zio effects - return first
  def sequenceTakeFirst[R,E,A,B](zioA: ZIO[R,E,A], zioB: ZIO[R,E,B]): ZIO[R,E,A] = for {
    a <- zioA
    _ <- zioB
  } yield (a) // same as zioA <* zioB
  
  // Run an ZIO forever
  def runForever[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,A] = zio.flatMap(_ => runForever(zio)) // tailrec
    
  // Convert value of ZIO to something else
  def convert[R,E,A,B](zio: ZIO[R,E,A], value: B): ZIO[R,E,B] = zio.map(_ => value)
  
  // Discard the value of ZIO to unit
  def asUnit[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,Unit] = for {
    _ <- zio
  } yield()
  
  val runtime = Runtime.default
  implicit val trace: Trace = Trace.empty
  
  Unsafe.unsafe { implicit u =>
    val firstEffect = ZIO.succeed {
      println("Computing first effect...")
      Thread.sleep(1000)
      1
    }
    
    val secondEffect = ZIO.succeed {
      println("Computing second effect...")
      Thread.sleep(1000)
      2
    }
    
    val evaluation = runtime.unsafe.run(sequenceTakeLast(firstEffect, secondEffect))
    print(evaluation)
    
    //val runningForever = runtime.unsafe.run(runForever(firstEffect))
  }
  
}