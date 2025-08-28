package arbo
package exchanges

import java.time.Instant

/**
 * Exchange health status data model for monitoring.
 * 
 * This case class represents the health status of an exchange,
 * including latency metrics, error rates, and last check time.
 */
case class ExchangeHealth(
  exchangeName: String,
  status: ExchangeStatus,
  latency: Option[Long], // in milliseconds
  errorRate: Option[Double], // percentage (0.0 to 1.0)
  lastChecked: Instant
)

/**
 * Enumeration of possible exchange health statuses.
 */
sealed trait ExchangeStatus

object ExchangeStatus {
  case object Healthy extends ExchangeStatus
  case object Unhealthy extends ExchangeStatus
  case object Degraded extends ExchangeStatus
  case object Unknown extends ExchangeStatus
}
