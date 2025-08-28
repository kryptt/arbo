package arbo

import kraken.{Config => KrakenApiConfig}
import exchanges.{ExchangeConfig, ExchangeRegistry}
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
      .eval(multiExchangeConfig.load[IO])
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

  /**
   * Multi-exchange configuration loading with Secret types and proper redaction.
   */
  def multiExchangeConfig: ConfigValue[Secret[ExchangeConfig]] = {
    ExchangeConfig.load[IO]
      .secret(ExchangeConfig.show)
  }

  /**
   * Legacy single-exchange configuration for backward compatibility.
   */
  def envConfig: ConfigValue[Secret[KrakenApiConfig]] =
    (env("API_KEY").redacted, envKey)
      .parMapN(KrakenApiConfig.apply)
      .secret(KrakenApiConfig.show)

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
