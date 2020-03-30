package arbo
package server

import data.{Ammount, Holding, HoldingSequence, SellSelection}

import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

import cats.effect.Sync
import cats.data.NonEmptyList
import cats.implicits._

object ArboRoutes {

  import kraken.{RestClient => KrakenAPI, CurrencyPair, KrakenOrder}

  def arbitrageOptions[F[_]: Sync](K: KrakenAPI[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    import SellSelection._

    val executeSelection: SellSelection[KrakenOrder] => F[HoldingSequence] = {
      case _: InitialState => Sync[F].raiseError(new Exception("No executions"))
      case SellPath(orders) => orders.traverse(K.execute)
      case NoSale(r) => Sync[F].raiseError(new Exception(r.toString))
    }

    HttpRoutes.of[F] {
      case GET -> Root / "options" =>
        K.assetPairs.flatMap(p => Ok(p.toString))

      case GET -> Root / "sales" :? params =>
        val balance: Ammount = params
          .get("eur")
          .foldMap(_.map(BigDecimal.apply).sum)
        K.sales(Holding("EUR", balance))
          .flatMap(s => Ok(s.toString))

      case GET -> Root / "rounds" :? params =>
        val balance: Ammount = params
          .get("eur")
          .foldMap(_.map(BigDecimal.apply).sum)
        val rounds: Int = params
          .get("count")
          .foldMap(_.map(_.toInt).sum)
        val round = K
          .sales(Holding("EUR", balance))
          .flatMap(executeSelection)

        (1 to rounds).toList
          .traverse(_ => round)
          .map(_.last)
          .flatMap(s => Ok(s.toString))
          .handleErrorWith(e => InternalServerError(e.getMessage))

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

}
