package arbo
package exchanges

import data._

import cats.data.NonEmptyList
import cats.effect.{Concurrent, Resource, Timer}
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.duration._
import java.time.Instant

/**
 * Multi-exchange management with health monitoring and registration logic.
 * 
 * The ExchangeRegistry manages multiple exchange instances, tracks their health,
 * and provides selection logic for choosing the best exchange for trading operations.
 * 
 * @tparam F The effect type (typically IO)
 * @tparam O The order type (must extend SellOrder)
 */
trait ExchangeRegistry[F[_], O <: SellOrder] {

  /**
   * Register a new exchange instance.
   * 
   * @param exchange The exchange to register
   * @return The registered exchange name
   */
  def register(exchange: Exchange[F, O]): F[String]

  /**
   * Unregister an exchange by name.
   * 
   * @param name The name of the exchange to unregister
   * @return True if the exchange was found and removed
   */
  def unregister(name: String): F[Boolean]

  /**
   * Get all registered exchanges.
   * 
   * @return A list of all registered exchanges
   */
  def getExchanges: F[List[Exchange[F, O]]]

  /**
   * Get a specific exchange by name.
   * 
   * @param name The name of the exchange
   * @return The exchange if found
   */
  def getExchange(name: String): F[Option[Exchange[F, O]]]

  /**
   * Get the health status of all registered exchanges.
   * 
   * @return A map of exchange names to their health status
   */
  def getHealthStatus: F[Map[String, ExchangeHealth]]

  /**
   * Get the health status of a specific exchange.
   * 
   * @param name The name of the exchange
   * @return The health status if the exchange is found
   */
  def getExchangeHealth(name: String): F[Option[ExchangeHealth]]

  /**
   * Select the best exchange for a given holding based on health, fees, and performance.
   * 
   * @param holding The holding to find the best exchange for
   * @return The best exchange if any are available
   */
  def selectBestExchange(holding: Holding): F[Option[Exchange[F, O]]]

  /**
   * Start periodic health monitoring for all registered exchanges.
   * 
   * @param interval The interval between health checks
   * @return A resource that manages the health monitoring background task
   */
  def startHealthMonitoring(interval: FiniteDuration): Resource[F, F[Unit]]

  /**
   * Get all healthy exchanges.
   * 
   * @return A list of exchanges that are currently healthy
   */
  def getHealthyExchanges: F[List[Exchange[F, O]]]
}

/**
 * In-memory implementation of ExchangeRegistry using Ref for state management.
 */
class InMemoryExchangeRegistry[F[_]: Concurrent: Timer, O <: SellOrder](
  private val exchanges: cats.effect.concurrent.Ref[F, Map[String, Exchange[F, O]]],
  private val healthStatus: cats.effect.concurrent.Ref[F, Map[String, ExchangeHealth]]
) extends ExchangeRegistry[F, O] {

  def register(exchange: Exchange[F, O]): F[String] = {
    exchanges.update(_ + (exchange.name -> exchange)) >> 
    healthStatus.update(_ + (exchange.name -> ExchangeHealth(
      exchangeName = exchange.name,
      status = ExchangeStatus.Unknown,
      latency = None,
      errorRate = None,
      lastChecked = Instant.now()
    ))) >>
    Concurrent[F].pure(exchange.name)
  }

  def unregister(name: String): F[Boolean] = {
    exchanges.modify { exs =>
      val wasPresent = exs.contains(name)
      (exs - name, wasPresent)
    }.flatMap { wasPresent =>
      if (wasPresent) {
        healthStatus.update(_ - name).as(true)
      } else {
        Concurrent[F].pure(false)
      }
    }
  }

  def getExchanges: F[List[Exchange[F, O]]] = 
    exchanges.get.map(_.values.toList)

  def getExchange(name: String): F[Option[Exchange[F, O]]] = 
    exchanges.get.map(_.get(name))

  def getHealthStatus: F[Map[String, ExchangeHealth]] = 
    healthStatus.get

  def getExchangeHealth(name: String): F[Option[ExchangeHealth]] = 
    healthStatus.get.map(_.get(name))

  def selectBestExchange(holding: Holding): F[Option[Exchange[F, O]]] = {
    getHealthyExchanges.flatMap { healthyExchanges =>
      if (healthyExchanges.isEmpty) {
        Concurrent[F].pure(None)
      } else {
        // For now, select the first healthy exchange
        // TODO: Implement more sophisticated selection logic based on fees, latency, etc.
        Concurrent[F].pure(healthyExchanges.headOption)
      }
    }
  }

  def startHealthMonitoring(interval: FiniteDuration): Resource[F, F[Unit]] = {
    val healthCheckTask = (Timer[F].sleep(interval) >> performHealthChecks).foreverM
    
    Resource.make(
      Concurrent[F].start(healthCheckTask)
    ) { fiber =>
      fiber.cancel
    }.map(_.join)
  }

  def getHealthyExchanges: F[List[Exchange[F, O]]] = {
    for {
      allExchanges <- getExchanges
      healthMap <- getHealthStatus
      healthyExchanges = allExchanges.filter { exchange =>
        healthMap.get(exchange.name).exists(_.status == ExchangeStatus.Healthy)
      }
    } yield healthyExchanges
  }

  private def performHealthChecks: F[Unit] = {
    getExchanges.flatMap { exchanges =>
      exchanges.traverse_ { exchange =>
        exchange.healthCheck
          .flatMap { health =>
            healthStatus.update(_ + (exchange.name -> health))
          }
          .handleError { _ =>
            val unhealthyHealth = ExchangeHealth(
              exchangeName = exchange.name,
              status = ExchangeStatus.Unhealthy,
              latency = None,
              errorRate = None,
              lastChecked = Instant.now()
            )
            healthStatus.update(_ + (exchange.name -> unhealthyHealth))
          }
      }
    }
  }
}

/**
 * Companion object for ExchangeRegistry providing factory methods.
 */
object ExchangeRegistry {

  /**
   * Create a new in-memory exchange registry.
   * 
   * @tparam F The effect type
   * @tparam O The order type
   * @return A resource containing the exchange registry
   */
  def inMemory[F[_]: Concurrent: Timer, O <: SellOrder]: Resource[F, ExchangeRegistry[F, O]] = {
    for {
      exchangesRef <- Resource.liftF(cats.effect.concurrent.Ref.of[F, Map[String, Exchange[F, O]]](Map.empty))
      healthRef <- Resource.liftF(cats.effect.concurrent.Ref.of[F, Map[String, ExchangeHealth]](Map.empty))
    } yield new InMemoryExchangeRegistry(exchangesRef, healthRef)
  }
}

  /**
   * Enhanced selectBestExchange method using sophisticated selection logic.
   */
  def selectBestExchangeWithMetrics[O <: SellOrder](
    holding: Holding,
    weights: ExchangeSelector.SelectionWeights = ExchangeSelector.SelectionWeights()
  ): F[Option[ExchangeSelector.ExchangeSelection[F, O]]] = {
    for {
      exchanges <- getExchanges
      healthStatus <- getHealthStatus
      selection <- ExchangeSelector.selectBestExchange(exchanges, holding, healthStatus, weights)
    } yield selection
  }
