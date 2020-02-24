package arbo
package kraken

import data._

import cache.Keep

import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.duration._

trait RestClient[F[_]] {
  def assetPairs: F[FeesResponse]
  def ticker(pair: NonEmptyList[CurrencyPair]): F[TickerResponse]
  def sales(holding: Holding): F[SellSelection]
}

object RestClient {

  val baseURI = uri"https://api.kraken.com/0/public/"

  import scalacache.CatsEffect.modes.async

  def apply[F[_]: Async](C: Client[F]): Resource[F, RestClient[F]] = for {
    feeCache <- Keep.cache[F, FeesResponse]
    tickerCache <- Keep.cache[F, TickerResponse]
  } yield new RestClient[F] {
    val dsl = new Http4sClientDsl[F]{}
    import dsl._

    def assetPairs: F[FeesResponse] = feeCache.cachingF("krakenKeeps")(Some(1.day)) {
      import FeesResponse._
      val req = GET(baseURI / "AssetPairs" +?("info", "fees"))
      C.expect[FeesResponse](req)
    }

    def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse] = tickerCache.cachingF("krakenTicker", pairs.sorted)(Some(1.minute)) {
      import TickerResponse._
      val pairparam = pairs.toList.map(CurrencyPair.toKraken).mkString(",")
      val req = GET(baseURI / "Ticker" +?("pair", pairparam))
      C.expect[TickerResponse](req)
    }

    def sales(holding: Holding): F[SellSelection] = {
      Calculator.selection(getSellOptions, "EUR", 6)(holding)
    }

    def getSellOptions(holding: Holding): F[SellOptions] = for {
      fees <- assetPairs
      candidates = fees.filter { case (k, _) => k.from == holding.currency || k.to == holding.currency }
      pairs <- NonEmptyList.fromList(candidates.keys.toList).fold[F[NonEmptyList[CurrencyPair]]](Async[F].raiseError(new Exception("no valid destination currencies")))(Async[F].pure)
      rates <- ticker(pairs)
    } yield rates.toList.map {
      case (CurrencyPair(from, to), ticker) =>
        SellOrder(from, to, (ticker.ask + ticker.bid ) / 2, holding.ammount, Fee(0, from))
    }
  }

}
