package arbo
package server

import kraken.{Config => ApiConfig}

import cats.effect.{ConcurrentEffect, Timer}
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object ArboServer {

  def stream[F[_]: ConcurrentEffect](config: ApiConfig, clientEC: ExecutionContext)(
      implicit T: Timer[F]): Stream[F, Nothing] = {
    for {
      client <- BlazeClientBuilder[F](clientEC)
        .withSocketKeepAlive(true)
        .withMaxTotalConnections(100)
        .withConnectTimeout(1.minute)
        .withResponseHeaderTimeout(5.minutes)
        .withIdleTimeout(30.minutes)
        .stream
      krakenAlg <- Stream.resource(kraken.RestClient[F](config, client))

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        ArboRoutes.arbitrageOptions[F](krakenAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(9000, "0.0.0.0")
        .withResponseHeaderTimeout(5.minutes)
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}
