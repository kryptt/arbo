package arbo
package kraken

import data._

import cache.Keep

import org.http4s.{Header, UrlForm, Uri}
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
import org.http4s.Charset

trait RestClient[F[_]] {
  def assetPairs: F[AssetPairsInfo]
  def ticker(pair: NonEmptyList[CurrencyPair]): F[TickerResponse]
  def sales(holding: Holding): F[SellSelection[KrakenOrder]]
  def execute(order: KrakenOrder): F[Holding]
}

object RestClient {

  val apiBaseURI = uri"https://api.kraken.com"
  val baseURI = uri"https://api.kraken.com/0/public/"

  val userVolume = 100000

  import scalacache.CatsEffect.modes.async

  def apply[F[_]: Async](config: Config, C: Client[F]): Resource[F, RestClient[F]] =
    for {
      feeCache <- Keep.cache[F, AssetPairsInfo]
      tickerCache <- Keep.cache[F, TickerResponse]
      salesCache <- Keep.cache[F, SellOptions[KrakenOrder]]
      nonce <- Resource.liftF(Keep.counter())
    } yield new RestClient[F] {
      val dsl = new Http4sClientDsl[F] {}
      import dsl._

      def assetPairs: F[AssetPairsInfo] = feeCache.cachingF("krakenAssetPairs")(Some(1.day)) {
        import AssetPairsInfo._
        val req = GET(baseURI / "AssetPairs" +? ("info", "info"))
        C.expect[AssetPairsInfo](req)
      }

      def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse] =
        tickerCache.cachingF("krakenTicker", pairs.sorted)(Some(1.minute)) {
          import TickerResponse._
          val pairparam = pairs.toList.map(CurrencyPair.toKraken).mkString(",")
          val req = GET(baseURI / "Ticker" +? ("pair", pairparam))
          C.expect[TickerResponse](req)
        }

      def sales(holding: Holding): F[SellSelection[KrakenOrder]] =
        Calculator.selection(getSellOptions, "EUR", 6)(holding)

      def execute(order: KrakenOrder): F[Holding] =
        nonce
          .getAndUpdate(_ + 1)
          .map { once =>
            val uri = "/0/private/AddOrder"
            val data = UrlForm(
              "nonce" -> once.toString,
              "type" -> order.krakenType,
              "pair" -> order.krakenPair,
              "ordertype" -> "limit",
              "price" -> order.krakenPrice,
              "volume" -> order.krakenVolume)
            val sign = Security.sign(once, data, uri, config.privateKey)
            val post = POST(data,
                            Uri.unsafeFromString(apiBaseURI.toString + uri),
                            Header("API-Key", config.apiKey),
                            Header("API-Sign", sign))
            println(order.getClass())
            println(UrlForm.encodeString(Charset.`UTF-8`)(data))
            post
          }
          .flatTap(post => Async[F].pure(println(post)))
          .flatMap(C.expect[Json])
          .flatTap(js => Async[F].pure(println(js)))
          .as(SellOrder.originalHolding(order))

      @inline def getSellOptions(holding: Holding): F[SellOptions[KrakenOrder]] =
        salesCache.cachingF("sellOptions", holding)(Some(1.minute))(for {
          fees <- assetPairs
          candidates = fees.filter {
            case (k, _) => CurrencyPair.holds(k, holding)
          }
          pairs <- Async[F].fromOption(
            NonEmptyList.fromList(candidates.keys.toList),
            new Exception("no valid destination currencies"))
          rates <- ticker(pairs)
        } yield rates.toList.map {
          case (cp @ CurrencyPair(base, cvar), ticker) if base == holding.currency =>
            val opts = fees(cp)
            Order.variableOrder(
              opts,
              fee = feeAmmount(opts.maker),
              basePrice = (ticker.bid + ticker.ask) / 2,
              holding = holding,
              to = cvar)
          case (cp @ CurrencyPair(base, cvar), ticker) if cvar == holding.currency =>
            val opts = fees(cp)
            Order.baseOrder(
              opts,
              fee = feeAmmount(opts.maker),
              basePrice = (ticker.bid + ticker.ask) / 2,
              holding = holding,
              to = base)
        })

      @inline def feeAmmount(makerFees: List[FeeOption]): Ammount =
        makerFees
          .findLast(_.volume < userVolume)
          .fold[Ammount](0.0024)(_.percentage / 100)

    }

}
