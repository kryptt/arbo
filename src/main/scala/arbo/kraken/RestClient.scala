package arbo
package kraken

import data._

import cache.Keep

import org.http4s.Header
import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._
import org.http4s.circe._

import io.circe.Json

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.duration._

trait RestClient[F[_]] {
  def assetPairs: F[FeesResponse]
  def ticker(pair: NonEmptyList[CurrencyPair]): F[TickerResponse]
  def sales(holding: Holding): F[SellSelection]
  def execute(order: SellOrder): F[Holding]
}

object RestClient {

  val baseURI = uri"https://api.kraken.com/0/public/"
  val privateURI = uri"https://api.kraken.com/0/private/"

  val userVolume = 100000

  import scalacache.CatsEffect.modes.async

  def apply[F[_]: Async](C: Client[F]): Resource[F, RestClient[F]] =
    for {
      feeCache <- Keep.cache[F, FeesResponse]
      tickerCache <- Keep.cache[F, TickerResponse]
      salesCache <- Keep.cache[F, SellOptions]
    } yield new RestClient[F] {
      val dsl = new Http4sClientDsl[F] {}
      import dsl._

      def assetPairs: F[FeesResponse] = feeCache.cachingF("krakenKeeps")(Some(1.day)) {
        import FeesResponse._
        val req = GET(baseURI / "AssetPairs" +? ("info", "fees"))
        C.expect[FeesResponse](req)
      }

      def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse] =
        tickerCache.cachingF("krakenTicker", pairs.sorted)(Some(1.minute)) {
          import TickerResponse._
          val pairparam = pairs.toList.map(CurrencyPair.toKraken).mkString(",")
          val req = GET(baseURI / "Ticker" +? ("pair", pairparam))
          C.expect[TickerResponse](req)
        }

      def sales(holding: Holding): F[SellSelection] =
        Calculator.selection(getSellOptions, "EUR", 6)(holding)

      def execute(order: SellOrder): F[Holding] = {
        val req = POST(
          baseURI / "AddOrder",
          Header("API-Key", "7jHdFe9VrH4bSsQrAnKjrRBGMeAZym3f4wCcIe2+tjF9mt4ZTzpcRUx7"))
        C.expect[Json](req)
          .flatTap(js => Async[F].pure(println(js)))
          .as(SellOrder.originalHolding(order))
      }

      @inline def getSellOptions(holding: Holding): F[SellOptions] =
        salesCache.cachingF("sellOptions", holding)(Some(1.minute))(for {
          fees <- assetPairs
          candidates = fees.filter {
            case (k, _) => k.from == holding.currency || k.to == holding.currency
          }
          pairs <- Async[F].fromOption(
            NonEmptyList.fromList(candidates.keys.toList),
            new Exception("no valid destination currencies"))
          rates <- ticker(pairs)
        } yield rates.toList.map {
          case (cp @ CurrencyPair(from, to), ticker) if from == holding.currency =>
            calcSellOrder(
              makerFees = fees(cp).maker,
              price = 2 / (ticker.bid + ticker.ask),
              holding = holding,
              to = to)
          case (cp @ CurrencyPair(from, to), ticker) if to == holding.currency =>
            calcSellOrder(
              makerFees = fees(cp).maker,
              price = (ticker.bid + ticker.ask) / 2,
              holding = holding,
              to = from)
        })

      @inline def calcSellOrder(
          makerFees: List[FeeOption],
          price: Price,
          holding: Holding,
          to: Currency): SellOrder = {
        val fee = makerFees
          .findLast(_.volume < userVolume)
          .fold[Ammount](0.0024)(_.percentage / 100)
        val from = holding.currency
        val ammount = holding.ammount / (1 + fee)
        val feeAmmount = ammount * fee
        SellOrder(from, to, price, ammount, Fee(feeAmmount, from))
      }
    }

}
