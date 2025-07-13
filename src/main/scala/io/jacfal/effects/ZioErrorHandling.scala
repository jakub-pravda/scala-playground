//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package jacfal.io.effects

import zio._
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.io.IOException

object ZioErrorHandling extends ZIOAppDefault {
  
  // failed ZIOs
  val failed = ZIO.fail("Some error")
  val anotherFailed = ZIO.fail(new IllegalArgumentException("Another error"))
  
  // handle errors with ZIO attempt, use it when you are unsure if the logic can ends with error
  val safeZIO: ZIO[Any, Throwable, Int] = ZIO.attempt {
    val someOp = 1
    throw new IllegalArgumentException("Op error")
  }
  
  val catchError = safeZIO.catchAll(error => ZIO.succeed("Some error occured during the processing"))
  val catchSelective = safeZIO.catchSome { 
    case e: IllegalArgumentException => ZIO.succeed("Ignoring IllegalArgumentException")
    case _ => ZIO.succeed("Ignoring everything else :)")
  }
  
  // effectfull fold with ZIO
  val effectfullFoldResult = safeZIO.foldZIO(
    ex => ZIO.succeed(s"Some error occured: $ex"),
    value => ZIO.succeed(s"Everything looking good! Result: $value")  
  )

  def evalTry[A](inputTry: Try[A]): Task[A] = {
    inputTry match {
      case Failure(exception) => ZIO.fail(exception)
      case Success(value) => ZIO.succeed(value)
    }
  } 
  
  // Error vs Defects?
  // Errors - Failures presents in the ZIO type signature
  // Defects - Failures not represented in the ZIO type signature (not recoverable, unforseen)

  // Example of defect:
  val divisionError: UIO[Int] = ZIO.succeed(4 / 0)
  
  // ZIO[R,E,A] can finish with:
  // - success containing A
  // - cause error containing E or unforseen throwable
  // -- Cause[A] can lead Fail[E] or Die(t: Throwable)
  
  val failedInt: ZIO[Any, String, Int] = ZIO.fail("Failed int!")
  failedInt.foldCause(
    cause => println(s"Some cause happened ${cause.defects}"),
    value => println(s"Some value produces $value")
  )
  
  // Works with defects
  def callHttpEndpoint: ZIO[Any, IOException, String] = ZIO.fail(new IOException("Something wrong"))
  def endpointCall: ZIO[Any, Nothing, String] = callHttpEndpoint.orDie // converting to defect
  
  // Some exercises:
  // 1. make this effect fail with a typed error
  val addFailure = ZIO.succeed[Int](throw new RuntimeException("This is bad!"))
  val betterFailure = addFailure.sandbox // exposes defect inro cause
  val betterFailure2 = addFailure.unrefine {
    case e => e
  }
  
  // 2. transform zio into a other ziowith a narower exception type
  def ioException[R,A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = zio.refineOrDie {
    case ioe: IOException => ioe
  }
  
  override def run = ???
}