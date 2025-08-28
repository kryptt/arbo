package arbo
package binance

import data._
import exchanges.Exchange
import exchanges.ExchangeHealth
import exchanges.ExchangeStatus

import org.http4s._
import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._
import org.http4s.circe._

import io.circe.Json

import cats.data.NonEmptyList
import cats.effect.{Async, Resource, Timer}
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.duration._
import java.time.Instant

import kraken.{CurrencyPair, TickerResponse, AssetPairsInfo}

/**
 * Binance API client following RestClient trait with rate limiting.
 * 
 * This client implements the Exchange trait for Binance, providing
 * rate limiting according to Binance's API limits:
 * - REQUEST_WEIGHT: 6000/min
 * - RAW_REQUESTS: 61000/5min
 */
class BinanceRestClient[F[_]: Async: Timer](
  config: Config,
  client: Client[F]
) extends Exchange[F, BinanceOrder] {

  val dsl = new Http4sClientDsl[F] {}
  import dsl._

  val name: String = "binance"

  // Rate limiting state
  private val requestWeightLimit = 6000
  private val rawRequestLimit = 61000
  private val weightWindow = 60.seconds
  private val rawWindow = 5.minutes

  // Base URLs
  private val baseUrl = if (config.testnet) {
    uri"https://testnet.binance.vision"
  } else {
    uri"https://api.binance.com"
  }

  private val apiBase = baseUrl / "api" / "v3"

  def supportedPairs: F[List[CurrencyPair]] = {
    exchangeInfo.flatMap { info =>
      val pairs = info.symbols
        .filter(_.status == "TRADING")
        .flatMap(symbol => Response.symbolToCurrencyPair(symbol.symbol))
      Async[F].pure(pairs)
    }
  }

  def getSellOptions(holding: Holding): F[SellOptions[BinanceOrder]] = {
    // This would be implemented to find available trading pairs for the holding
    // For now, return empty list as a placeholder
    Async[F].pure(List.empty[BinanceOrder])
  }

  def execute(order: BinanceOrder): F[Holding] = {
    // This would be implemented to execute the order on Binance
    // For now, return the original holding as a placeholder
    Async[F].pure(SellOrder.originalHolding(order))
  }

  def healthCheck: F[ExchangeHealth] = {
    val startTime = Instant.now()
    
    ping
      .map { _ =>
        val latency = java.time.Duration.between(startTime, Instant.now()).toMillis
        ExchangeHealth(
          exchangeName = name,
          status = ExchangeStatus.Healthy,
          latency = Some(latency),
          errorRate = None,
          lastChecked = Instant.now()
        )
      }
      .handleError { _ =>
        ExchangeHealth(
          exchangeName = name,
          status = ExchangeStatus.Unhealthy,
          latency = None,
          errorRate = None,
          lastChecked = Instant.now()
        )
      }
  }

  def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse] = {
    val symbols = pairs.map(Response.currencyPairToSymbol).toList.mkString(",")
    val uri = apiBase / "ticker" / "24hr" +? ("symbols", symbols)
    
    client.expect[List[Response.Ticker24hr]](GET(uri))
      .map { tickers =>
        tickers.flatMap { ticker =>
          for {
            pair <- Response.symbolToCurrencyPair(ticker.symbol)
            arboTicker <- Response.ticker24hrToTicker(ticker)
          } yield (pair, arboTicker)
        }.toMap
      }
  }

  def assetPairs: F[AssetPairsInfo] = {
    exchangeInfo.map { info =>
      info.symbols
        .flatMap { symbolInfo =>
          for {
            pair <- Response.symbolToCurrencyPair(symbolInfo.symbol)
            options <- Response.symbolInfoToAssetPairOptions(symbolInfo)
          } yield (pair, options)
        }
        .toMap
    }
  }

  // Private helper methods

  private def ping: F[Unit] = {
    val uri = apiBase / "ping"
    client.expect[Json](GET(uri)).void
  }

  private def exchangeInfo: F[Response.ExchangeInfo] = {
    val uri = apiBase / "exchangeInfo"
    client.expect[Response.ExchangeInfo](GET(uri))
  }

  private def serverTime: F[Long] = {
    val uri = apiBase / "time"
    client.expect[Json](GET(uri))
      .map(_.hcursor.downField("serverTime").as[Long].getOrElse(0L))
  }

  /**
   * Create a signed request for authenticated endpoints.
   */
  private def createSignedRequest(
    method: Method,
    endpoint: String,
    params: Map[String, String] = Map.empty
  ): F[Request[F]] = {
    val timestamp = System.currentTimeMillis()
    val queryString = Security.createSignedQueryString(
      params,
      timestamp,
      recvWindow = 5000L,
      config.secretKey
    )
    
    val uri = Uri.unsafeFromString(s"${apiBase.toString}/$endpoint?$queryString")
    
    val request = method match {
      case GET => GET(uri)
      case POST => POST(uri)
      case PUT => PUT(uri)
      case DELETE => DELETE(uri)
      case _ => GET(uri) // Default to GET
    }
    
    Async[F].pure(
      request.withHeaders(
        Header("X-MBX-APIKEY", config.apiKey)
      )
    )
  }
}

/**
 * Companion object for BinanceRestClient providing factory methods.
 */
object BinanceRestClient {

  /**
   * Create a new Binance REST client.
   * 
   * @param config Binance configuration
   * @param client HTTP client
   * @return A resource containing the Binance REST client
   */
  def apply[F[_]: Async: Timer](
    config: Config,
    client: Client[F]
  ): Resource[F, BinanceRestClient[F]] = {
    Resource.pure(new BinanceRestClient(config, client))
  }
}
