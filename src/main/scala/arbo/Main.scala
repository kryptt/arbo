package arbo

import kraken.{Config => ApiConfig}
import server.ArboServer

import ciris._

import fs2.Stream

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.Stream
import ciris.{ConfigValue, env}

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Main extends IOApp {

  def run(args: List[String]) =
    Stream.eval(envConfig.load[IO])
      .zip(Stream.resource(mainEC))
      .flatMap(Function.tupled(ArboServer.stream[IO] _))
      .compile.drain.as(ExitCode.Success)

  def mainEC: Resource[IO, ExecutionContext] = Resource {
    IO.delay {
      val main = Executors.newWorkStealingPool()
      ExecutionContext.fromExecutorService(main) -> IO.delay(main.shutdown())
    }
  }

  def envConfig: ConfigValue[ApiConfig] =
    (env("API_KEY"), env("PRIVATE_KEY")).parMapN(ApiConfig.apply)

  def cfg: ConfigValue[kraken.Config] =
    (env("API_KEY"), env("PRIVATE_KEY"))
      .parMapN(kraken.Config)
}
