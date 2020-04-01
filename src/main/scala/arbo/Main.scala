package arbo

import kraken.{Config => ApiConfig}
import server.ArboServer

import ciris._

import fs2.Stream

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{Stream, text}
import ciris.{ConfigValue, env}
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Main extends IOApp {

  def run(args: List[String]) =
    Stream
      .eval(envConfig.load[IO])
      .map(_.value)
      .zip(Stream.resource(mainEC))
      .flatMap(Function.tupled(ArboServer.stream[IO] _))
      .compile
      .drain
      .as(ExitCode.Success)

  def mainEC: Resource[IO, ExecutionContext] = Resource {
    IO.delay {
      val main = Executors.newWorkStealingPool()
      ExecutionContext.fromExecutorService(main) -> IO.delay(main.shutdown())
    }
  }

  def envConfig: ConfigValue[Secret[ApiConfig]] =
    (env("API_KEY").redacted, envKey)
      .parMapN(ApiConfig.apply)
      .secret(ApiConfig.show)

  def envKey: ConfigValue[ByteVector] =
    env("PRIVATE_KEY")
      .evalMap(
        Stream
          .emit(_)
          .covary[IO]
          .through(text.base64Decode)
          .compile
          .to(ByteVector))
      .redacted

}
