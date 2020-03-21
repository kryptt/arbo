package arbo

import arbo.server.ArboServer
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream
import ciris.{ConfigValue, env}

object Main extends IOApp {
  def run(args: List[String]) =
    Stream.eval(cfg.load[IO])
      .flatMap(ArboServer.stream[IO])
      .compile
      .drain
      .as(ExitCode.Success)

  def cfg: ConfigValue[kraken.Config] =
    (env("API_KEY"), env("PRIVATE_KEY"))
      .parMapN(kraken.Config)
}
