package arbo

import arbo.server.ArboServer
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Main extends IOApp {
  def run(args: List[String]) =
    ArboServer.stream[IO].compile.drain.as(ExitCode.Success)
}
