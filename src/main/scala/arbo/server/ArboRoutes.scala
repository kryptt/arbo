package arbo
package server

import data.{Ammount, Holding}

import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

import cats.effect.Sync
import cats.data.NonEmptyList
import cats.implicits._

object ArboRoutes {

  import kraken.{RestClient => KrakenAPI, CurrencyPair}

  def arbitrageOptions[F[_]: Sync](K: KrakenAPI[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "options" =>
        K.assetPairs.flatMap(p => Ok(p.toString))
      case GET -> Root / "sales" :? params =>
        val balance: Ammount = params
          .get("eur")
          .foldMap(_.map(BigDecimal.apply).sum)
        K.sales(Holding("EUR", balance))
          .flatMap(s => Ok(s.toString))

      case GET -> Root / "ticker" :? params =>
        params
          .get("pair")
          .map(_.toList.flatMap(_.split(",")).flatMap(CurrencyPair.fromString))
          .flatMap(NonEmptyList.fromList)
          .traverse(K.ticker _)
          .flatMap {
            case Some(p) => Ok(p.toString)
            case None => BadRequest("No params specified")
          }
    }
  }

  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }
}
