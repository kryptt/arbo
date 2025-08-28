package arbo
package exchanges

import cats.effect.{Concurrent, Timer}
import cats.syntax.applicativeError._
import cats.syntax.functor._

import scala.concurrent.duration._
import java.time.Instant

/**
 * Circuit breaker pattern implementation for exchange health monitoring.
 * 
 * The circuit breaker automatically disables exchanges that are experiencing
 * high error rates or poor performance, and re-enables them after a recovery period.
 */
sealed trait CircuitBreakerState

object CircuitBreakerState {
  case object Closed extends CircuitBreakerState    // Normal operation
  case object Open extends CircuitBreakerState      // Circuit is open, requests fail fast
  case object HalfOpen extends CircuitBreakerState  // Testing if service has recovered
}

/**
 * Configuration for circuit breaker behavior.
 */
case class CircuitBreakerConfig(
  failureThreshold: Int = 5,           // Number of failures before opening circuit
  successThreshold: Int = 3,           // Number of successes needed to close circuit from half-open
  timeout: FiniteDuration = 30.seconds, // Time to wait before trying half-open
  maxFailures: Int = 10,               // Maximum failures to track
  errorRateThreshold: Double = 0.5     // Error rate threshold (0.0 to 1.0)
)

/**
 * Circuit breaker state tracking.
 */
case class CircuitBreakerMetrics(
  state: CircuitBreakerState,
  failureCount: Int,
  successCount: Int,
  lastFailureTime: Option[Instant],
  lastSuccessTime: Option[Instant],
  totalRequests: Int,
  totalFailures: Int
) {
  def errorRate: Double = {
    if (totalRequests == 0) 0.0
    else totalFailures.toDouble / totalRequests.toDouble
  }
  
  def isHealthy: Boolean = state == CircuitBreakerState.Closed
  
  def shouldOpen(config: CircuitBreakerConfig): Boolean = {
    state == CircuitBreakerState.Closed && (
      failureCount >= config.failureThreshold ||
      errorRate >= config.errorRateThreshold
    )
  }
  
  def shouldClose(config: CircuitBreakerConfig): Boolean = {
    state == CircuitBreakerState.HalfOpen && successCount >= config.successThreshold
  }
  
  def shouldAttemptHalfOpen(config: CircuitBreakerConfig): Boolean = {
    state == CircuitBreakerState.Open && 
    lastFailureTime.exists { lastFailure =>
      Instant.now().toEpochMilli - lastFailure.toEpochMilli >= config.timeout.toMillis
    }
  }
}

/**
 * Circuit breaker implementation for exchange operations.
 */
class ExchangeCircuitBreaker[F[_]: Concurrent: Timer](
  private val config: CircuitBreakerConfig,
  private val metrics: cats.effect.concurrent.Ref[F, CircuitBreakerMetrics]
) {

  /**
   * Execute an operation with circuit breaker protection.
   * 
   * @param operation The operation to execute
   * @return The result of the operation or a circuit breaker error
   */
  def execute[A](operation: F[A]): F[A] = {
    metrics.get.flatMap { currentMetrics =>
      currentMetrics.state match {
        case CircuitBreakerState.Closed =>
          executeWithMetrics(operation)
        case CircuitBreakerState.Open =>
          if (currentMetrics.shouldAttemptHalfOpen(config)) {
            attemptHalfOpen(operation)
          } else {
            Concurrent[F].raiseError(new CircuitBreakerOpenException(
              s"Circuit breaker is open. Last failure: ${currentMetrics.lastFailureTime}"
            ))
          }
        case CircuitBreakerState.HalfOpen =>
          executeWithMetrics(operation)
      }
    }
  }

  /**
   * Get current circuit breaker metrics.
   */
  def getMetrics: F[CircuitBreakerMetrics] = metrics.get

  /**
   * Reset the circuit breaker to closed state.
   */
  def reset: F[Unit] = {
    metrics.set(CircuitBreakerMetrics(
      state = CircuitBreakerState.Closed,
      failureCount = 0,
      successCount = 0,
      lastFailureTime = None,
      lastSuccessTime = None,
      totalRequests = 0,
      totalFailures = 0
    ))
  }

  private def executeWithMetrics[A](operation: F[A]): F[A] = {
    val startTime = Instant.now()
    
    operation
      .flatTap { _ =>
        recordSuccess(startTime)
      }
      .handleErrorWith { error =>
        recordFailure(startTime)
          .flatMap { newMetrics =>
            if (newMetrics.shouldOpen(config)) {
              openCircuit()
            } else {
              Concurrent[F].unit
            }
          }
          .flatMap { _ =>
            Concurrent[F].raiseError(error)
          }
      }
  }

  private def attemptHalfOpen[A](operation: F[A]): F[A] = {
    metrics.update(_.copy(state = CircuitBreakerState.HalfOpen, successCount = 0))
    executeWithMetrics(operation)
  }

  private def recordSuccess(startTime: Instant): F[Unit] = {
    metrics.update { current =>
      val newMetrics = current.copy(
        successCount = current.successCount + 1,
        lastSuccessTime = Some(Instant.now()),
        totalRequests = current.totalRequests + 1
      )
      
      if (newMetrics.shouldClose(config)) {
        newMetrics.copy(
          state = CircuitBreakerState.Closed,
          failureCount = 0,
          successCount = 0
        )
      } else {
        newMetrics
      }
    }
  }

  private def recordFailure(startTime: Instant): F[CircuitBreakerMetrics] = {
    metrics.updateAndGet { current =>
      val newMetrics = current.copy(
        failureCount = current.failureCount + 1,
        lastFailureTime = Some(Instant.now()),
        totalRequests = current.totalRequests + 1,
        totalFailures = current.totalFailures + 1
      )
      
      // Keep only recent failures to prevent unbounded growth
      if (newMetrics.totalFailures > config.maxFailures) {
        newMetrics.copy(
          totalFailures = config.maxFailures,
          totalRequests = math.max(config.maxFailures, newMetrics.totalRequests)
        )
      } else {
        newMetrics
      }
    }
  }

  private def openCircuit(): F[Unit] = {
    metrics.update(_.copy(state = CircuitBreakerState.Open))
  }
}

/**
 * Circuit breaker exceptions.
 */
class CircuitBreakerOpenException(message: String) extends RuntimeException(message)
class CircuitBreakerTimeoutException(message: String) extends RuntimeException(message)

/**
 * Companion object for ExchangeCircuitBreaker providing factory methods.
 */
object ExchangeCircuitBreaker {

  /**
   * Create a new circuit breaker with default configuration.
   */
  def create[F[_]: Concurrent: Timer]: F[ExchangeCircuitBreaker[F]] = {
    create(CircuitBreakerConfig())
  }

  /**
   * Create a new circuit breaker with custom configuration.
   */
  def create[F[_]: Concurrent: Timer](
    config: CircuitBreakerConfig
  ): F[ExchangeCircuitBreaker[F]] = {
    val initialMetrics = CircuitBreakerMetrics(
      state = CircuitBreakerState.Closed,
      failureCount = 0,
      successCount = 0,
      lastFailureTime = None,
      lastSuccessTime = None,
      totalRequests = 0,
      totalFailures = 0
    )
    
    cats.effect.concurrent.Ref.of[F, CircuitBreakerMetrics](initialMetrics)
      .map(new ExchangeCircuitBreaker(config, _))
  }
}
