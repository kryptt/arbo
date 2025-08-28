package arbo
package exchanges

import data._
import kraken.{CurrencyPair, TickerResponse, AssetPairsInfo}

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.functor._

/**
 * Abstract trait defining unified exchange interface for both Kraken and Binance.
 * 
 * This trait provides a common interface that all exchange implementations must follow,
 * enabling the Calculator to work with multiple exchanges seamlessly.
 * 
 * @tparam F The effect type (typically IO)
 * @tparam O The order type specific to this exchange (e.g., KrakenOrder, BinanceOrder)
 */
trait Exchange[F[_], O <: SellOrder] {

  /**
   * The name of this exchange (e.g., "kraken", "binance").
   */
  def name: String

  /**
   * Get the list of currency pairs supported by this exchange.
   * 
   * @return A list of supported currency pairs
   */
  def supportedPairs: F[List[CurrencyPair]]

  /**
   * Get available sell options for a given holding.
   * 
   * This method should return all possible sell orders that can be executed
   * for the given holding, considering the exchange's supported pairs and
   * current market conditions.
   * 
   * @param holding The holding to find sell options for
   * @return A list of possible sell orders
   */
  def getSellOptions(holding: Holding): F[SellOptions[O]]

  /**
   * Execute a sell order on this exchange.
   * 
   * @param order The order to execute
   * @return The resulting holding after execution
   */
  def execute(order: O): F[Holding]

  /**
   * Check the health status of this exchange.
   * 
   * This method should perform a lightweight health check to determine
   * if the exchange is currently available and responsive.
   * 
   * @return The health status of the exchange
   */
  def healthCheck: F[ExchangeHealth]

  /**
   * Get the current ticker information for specified currency pairs.
   * 
   * @param pairs The currency pairs to get ticker data for
   * @return Ticker response containing current market data
   */
  def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse]

  /**
   * Get asset pairs information including fees and trading rules.
   * 
   * @return Asset pairs information
   */
  def assetPairs: F[AssetPairsInfo]
}

/**
 * Companion object for Exchange trait providing common utilities.
 */
object Exchange {

  /**
   * Create a simple health check that pings the exchange's basic endpoint.
   * 
   * @param name The exchange name
   * @param healthCheckF The actual health check function
   * @return A health check result
   */
  def simpleHealthCheck[F[_]: Sync](
    name: String,
    healthCheckF: F[Boolean]
  ): F[ExchangeHealth] = {
    import cats.syntax.applicativeError._
    import cats.syntax.functor._
    
    healthCheckF
      .map { isHealthy =>
        ExchangeHealth(
          exchangeName = name,
          status = if (isHealthy) ExchangeStatus.Healthy else ExchangeStatus.Unhealthy,
          latency = None, // Could be measured if needed
          errorRate = None, // Could be tracked if needed
          lastChecked = java.time.Instant.now()
        )
      }
      .handleError { _ =>
        ExchangeHealth(
          exchangeName = name,
          status = ExchangeStatus.Unhealthy,
          latency = None,
          errorRate = None,
          lastChecked = java.time.Instant.now()
        )
      }
  }
}
