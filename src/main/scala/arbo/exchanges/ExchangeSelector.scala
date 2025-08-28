package arbo
package exchanges

import data._

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor._
import cats.syntax.traverse._

import java.time.Instant
import scala.concurrent.duration._

/**
 * Exchange selection criteria and scoring logic.
 * 
 * This object provides utilities for scoring and selecting exchanges
 * based on various criteria like profit potential, fees, response times, and success rates.
 */
object ExchangeSelector {

  /**
   * Configuration for exchange selection criteria weights.
   */
  case class SelectionWeights(
    profitWeight: Double = 0.4,      // Weight for profit potential
    feeWeight: Double = 0.3,         // Weight for fee considerations
    latencyWeight: Double = 0.2,     // Weight for response time
    reliabilityWeight: Double = 0.1  // Weight for success rate
  ) {
    require(
      (profitWeight + feeWeight + latencyWeight + reliabilityWeight - 1.0).abs < 0.001,
      "Weights must sum to approximately 1.0"
    )
  }

  /**
   * Exchange performance metrics for selection.
   */
  case class ExchangeMetrics(
    exchangeName: String,
    profitScore: Double,        // 0.0 to 1.0, higher is better
    feeScore: Double,          // 0.0 to 1.0, higher is better (lower fees)
    latencyScore: Double,      // 0.0 to 1.0, higher is better (lower latency)
    reliabilityScore: Double,  // 0.0 to 1.0, higher is better (higher success rate)
    lastUpdated: Instant
  ) {
    /**
     * Calculate overall score using weighted criteria.
     */
    def overallScore(weights: SelectionWeights): Double = {
      profitScore * weights.profitWeight +
      feeScore * weights.feeWeight +
      latencyScore * weights.latencyWeight +
      reliabilityScore * weights.reliabilityWeight
    }
  }

  /**
   * Exchange selection result.
   */
  case class ExchangeSelection[F[_], O <: SellOrder](
    selectedExchange: Exchange[F, O],
    score: Double,
    metrics: ExchangeMetrics,
    alternatives: List[(Exchange[F, O], Double, ExchangeMetrics)]
  )

  /**
   * Calculate profit score based on potential profit from sell options.
   * 
   * @param sellOptions Available sell options from the exchange
   * @param holding The holding to evaluate
   * @return Profit score between 0.0 and 1.0
   */
  def calculateProfitScore[O <: SellOrder](
    sellOptions: SellOptions[O],
    holding: Holding
  ): Double = {
    if (sellOptions.isEmpty) return 0.0
    
    val profits = sellOptions.flatMap { order =>
      SellOrder.toAmmount(order).map { finalAmount =>
        (finalAmount - holding.ammount) / holding.ammount
      }
    }
    
    if (profits.isEmpty) return 0.0
    
    val maxProfit = profits.max
    val minProfit = profits.min
    
    // Normalize profit to 0-1 scale, with 0% profit = 0.5, positive profits > 0.5
    if (maxProfit == minProfit) 0.5
    else {
      val normalized = (maxProfit - minProfit) / (maxProfit - minProfit + 1.0)
      math.max(0.0, math.min(1.0, normalized + 0.5))
    }
  }

  /**
   * Calculate fee score based on trading fees.
   * 
   * @param sellOptions Available sell options with fee information
   * @return Fee score between 0.0 and 1.0 (higher = lower fees)
   */
  def calculateFeeScore[O <: SellOrder](sellOptions: SellOptions[O]): Double = {
    if (sellOptions.isEmpty) return 0.0
    
    val fees = sellOptions.map { order =>
      // Convert fee to percentage of order value
      if (order.fee.currency == order.from) {
        order.fee.ammount / order.fromAmmount
      } else {
        order.fee.ammount / (order.fromAmmount / order.price)
      }
    }
    
    val avgFee = fees.sum / fees.length
    val maxFee = fees.max
    
    // Score inversely proportional to fees (lower fees = higher score)
    if (maxFee == 0.0) 1.0
    else math.max(0.0, 1.0 - (avgFee / maxFee))
  }

  /**
   * Calculate latency score based on exchange response time.
   * 
   * @param latencyMs Latency in milliseconds
   * @return Latency score between 0.0 and 1.0 (lower latency = higher score)
   */
  def calculateLatencyScore(latencyMs: Option[Long]): Double = {
    latencyMs match {
      case None => 0.5 // Unknown latency gets neutral score
      case Some(latency) =>
        // Score inversely proportional to latency
        // 0ms = 1.0, 1000ms = 0.0, with exponential decay
        math.max(0.0, math.exp(-latency / 500.0))
    }
  }

  /**
   * Calculate reliability score based on error rate and health status.
   * 
   * @param health Exchange health information
   * @return Reliability score between 0.0 and 1.0
   */
  def calculateReliabilityScore(health: ExchangeHealth): Double = {
    val statusScore = health.status match {
      case ExchangeStatus.Healthy => 1.0
      case ExchangeStatus.Degraded => 0.7
      case ExchangeStatus.Unhealthy => 0.0
      case ExchangeStatus.Unknown => 0.5
    }
    
    val errorRateScore = health.errorRate match {
      case None => 0.5 // Unknown error rate gets neutral score
      case Some(rate) => math.max(0.0, 1.0 - rate)
    }
    
    (statusScore + errorRateScore) / 2.0
  }

  /**
   * Select the best exchange from a list of candidates.
   * 
   * @param exchanges List of exchanges to evaluate
   * @param holding The holding to find the best exchange for
   * @param healthStatus Health status of all exchanges
   * @param weights Selection criteria weights
   * @return The best exchange selection result
   */
  def selectBestExchange[F[_]: Concurrent, O <: SellOrder](
    exchanges: List[Exchange[F, O]],
    holding: Holding,
    healthStatus: Map[String, ExchangeHealth],
    weights: SelectionWeights = SelectionWeights()
  ): F[Option[ExchangeSelection[O]]] = {
    
    if (exchanges.isEmpty) {
      Concurrent[F].pure(None)
    } else {
      // Filter to only healthy exchanges
      val healthyExchanges = exchanges.filter { exchange =>
        healthStatus.get(exchange.name).exists(_.status == ExchangeStatus.Healthy)
      }
      
      if (healthyExchanges.isEmpty) {
        Concurrent[F].pure(None)
      } else {
        // Evaluate each exchange
        healthyExchanges.traverse { exchange =>
          for {
            sellOptions <- exchange.getSellOptions(holding)
            health <- Concurrent[F].fromOption(
              healthStatus.get(exchange.name),
              new RuntimeException(s"Health status not found for ${exchange.name}")
            )
          } yield {
            val profitScore = calculateProfitScore(sellOptions, holding)
            val feeScore = calculateFeeScore(sellOptions)
            val latencyScore = calculateLatencyScore(health.latency)
            val reliabilityScore = calculateReliabilityScore(health)
            
            val metrics = ExchangeMetrics(
              exchangeName = exchange.name,
              profitScore = profitScore,
              feeScore = feeScore,
              latencyScore = latencyScore,
              reliabilityScore = reliabilityScore,
              lastUpdated = Instant.now()
            )
            
            val overallScore = metrics.overallScore(weights)
            (exchange, overallScore, metrics)
          }
        }.map { evaluations =>
          if (evaluations.isEmpty) {
            None
          } else {
            val sorted = evaluations.sortBy(-_._2) // Sort by score descending
            val (bestExchange, bestScore, bestMetrics) = sorted.head
            val alternatives = sorted.tail.map { case (ex, score, metrics) => (ex, score, metrics) }
            
            Some(ExchangeSelection(
              selectedExchange = bestExchange,
              score = bestScore,
              metrics = bestMetrics,
              alternatives = alternatives
            ))
          }
        }
      }
    }
  }
}
