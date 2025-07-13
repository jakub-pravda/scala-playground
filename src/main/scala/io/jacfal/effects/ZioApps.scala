//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package jacfal.io.effects

import zio._

// run me with scala cli ->
//  scala-cli -S 2.13.16 src/main/scala/io/jacfal/effects/ZioApps.scala  
object ZioApps extends ZIOAppDefault {
  // provides runtime, trace... by default
  override def run = ZIO.succeed(println("Hello from ZIO runtime!"))
}