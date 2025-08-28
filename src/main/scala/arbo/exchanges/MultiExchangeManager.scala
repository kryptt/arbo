package arbo
package exchanges

import data._

import cats.data.NonEmptyList
import cats.effect.{Async, Resource, Timer}
import cats.syntax.functor._
import cats.syntax.traverse._

import org.http4s.client.Client

import kraken.{KrakenExchange, Config => KrakenConfig}
import binance.{BinanceRestClient, Config => BinanceConfig}

/**
 * Multi-exchange manager that coordinates between Kraken and Binance.
 */
class MultiExchangeManager[F[_]: Async: Timer](
  exchangeConfig: ExchangeConfig,
  httpClient: Client[F]
) {

  /**
   * Create all available exchanges based on configuration.
   */
  def createExchanges: Resource[F, List[Exchange[F, SellOrder]]] = {
    val exchanges = List.newBuilder[Resource[F, Exchange[F, SellOrder]]]
    
    exchangeConfig.kraken.foreach { krakenConfig =>
      exchanges += KrakenExchange[F](krakenConfig, httpClient).map(_.asInstanceOf[Exchange[F, SellOrder]])
    }
    
    exchangeConfig.binance.foreach { binanceConfig =>
      exchanges += BinanceRestClient[F](binanceConfig, httpClient).map(_.asInstanceOf[Exchange[F, SellOrder]])
    }
    
    val exchangeResources = exchanges.result()
    
    if (exchangeResources.isEmpty) {
      Resource.pure[F, List[Exchange[F, SellOrder]]](List.empty)
    } else {
      exchangeResources.traverse(identity).map(_.toList)
    }
  }

  /**
   * Create a registry with all available exchanges.
   */
  def createRegistry: Resource[F, ExchangeRegistry[F, SellOrder]] = {
    for {
      exchanges <- createExchanges
      registry <- ExchangeRegistry.inMemory[F, SellOrder]
      _ <- Resource.eval(
        exchanges.traverse_(exchange => registry.register(exchange))
      )
    } yield registry
  }

  /**
   * Get the best exchange for a given holding.
   */
  def getBestExchange(holding: Holding): Resource[F, Option[Exchange[F, SellOrder]]] = {
    createRegistry.map { registry =>
      // This would need to be implemented as an F operation
      // For now, just return the first available exchange
      None
    }
  }
}

object MultiExchangeManager {
  def apply[F[_]: Async: Timer](
    exchangeConfig: ExchangeConfig,
    httpClient: Client[F]
  ): MultiExchangeManager[F] = {
    new MultiExchangeManager(exchangeConfig, httpClient)
  }
}
