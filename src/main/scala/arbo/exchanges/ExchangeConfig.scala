package arbo
package exchanges

import data._

import cats.Show
import ciris._
import ciris.cats.effect._
import ciris.refined._

import scala.concurrent.duration._

/**
 * Multi-exchange configuration management.
 * 
 * This module provides configuration classes for managing multiple exchanges,
 * including Kraken and Binance configurations with preferences and environment variable support.
 */
case class ExchangeConfig(
  kraken: Option[kraken.Config],
  binance: Option[binance.Config],
  preferences: ExchangePreferences
)

object ExchangeConfig {

  implicit val show: Show[ExchangeConfig] = Show.show { config =>
    s"ExchangeConfig(kraken=${config.kraken.isDefined}, binance=${config.binance.isDefined}, preferences=${config.preferences})"
  }

  /**
   * Load exchange configuration from environment variables.
   */
  def load[F[_]: Async]: ConfigValue[F, ExchangeConfig] = {
    (
      loadKrakenConfig,
      loadBinanceConfig,
      loadExchangePreferences
    ).parMapN(ExchangeConfig.apply)
  }

  private def loadKrakenConfig[F[_]: Async]: ConfigValue[F, Option[kraken.Config]] = {
    (
      env("KRAKEN_API_KEY").as[String].option,
      env("KRAKEN_PRIVATE_KEY").as[String].option
    ).parMapN { (apiKey, privateKey) =>
      (apiKey, privateKey).mapN { (key, priv) =>
        kraken.Config(key, scodec.bits.ByteVector.fromBase64(priv).get)
      }
    }
  }

  private def loadBinanceConfig[F[_]: Async]: ConfigValue[F, Option[binance.Config]] = {
    (
      env("BINANCE_API_KEY").as[String].option,
      env("BINANCE_SECRET_KEY").as[String].option,
      env("BINANCE_TESTNET").as[Boolean].option.map(_.getOrElse(false))
    ).parMapN { (apiKey, secretKey, testnet) =>
      (apiKey, secretKey).mapN { (key, secret) =>
        binance.Config(key, scodec.bits.ByteVector.fromBase64(secret).get, testnet)
      }
    }
  }

  private def loadExchangePreferences[F[_]: Async]: ConfigValue[F, ExchangePreferences] = {
    (
      env("EXCHANGE_TIMEOUT_MS").as[Long].option.map(_.map(_.milliseconds)),
      env("MAX_DEPTH_PER_EXCHANGE").as[Int].option,
      env("PREFERRED_EXCHANGES").as[String].option.map(_.map(_.split(",").toList)),
      env("MIN_PROFIT_THRESHOLD").as[Double].option,
      env("EXCHANGE_SELECTION_WEIGHTS").as[String].option
    ).parMapN { (timeout, maxDepth, preferred, minProfit, weights) =>
      ExchangePreferences(
        timeout = timeout.getOrElse(30.seconds),
        maxDepthPerExchange = maxDepth.getOrElse(6),
        preferredExchanges = preferred.getOrElse(List("kraken", "binance")),
        minProfitThreshold = minProfit.getOrElse(0.001), // 0.1% default
        selectionWeights = weights.flatMap(parseSelectionWeights).getOrElse(SelectionWeights())
      )
    }
  }

  private def parseSelectionWeights(weightsStr: String): Option[SelectionWeights] = {
    // Parse weights from string like "profit:0.4,fee:0.3,latency:0.2,reliability:0.1"
    try {
      val weights = weightsStr.split(",").map { pair =>
        val Array(key, value) = pair.split(":")
        key.trim -> value.trim.toDouble
      }.toMap

      Some(SelectionWeights(
        profitWeight = weights.getOrElse("profit", 0.4),
        feeWeight = weights.getOrElse("fee", 0.3),
        latencyWeight = weights.getOrElse("latency", 0.2),
        reliabilityWeight = weights.getOrElse("reliability", 0.1)
      ))
    } catch {
      case _: Exception => None
    }
  }
}

/**
 * Exchange preferences configuration.
 */
case class ExchangePreferences(
  timeout: FiniteDuration = 30.seconds,
  maxDepthPerExchange: Int = 6,
  preferredExchanges: List[String] = List("kraken", "binance"),
  minProfitThreshold: Double = 0.001, // 0.1%
  selectionWeights: SelectionWeights = SelectionWeights()
)

object ExchangePreferences {
  implicit val show: Show[ExchangePreferences] = Show.show { prefs =>
    s"ExchangePreferences(timeout=${prefs.timeout}, maxDepth=${prefs.maxDepthPerExchange}, " +
    s"preferred=${prefs.preferredExchanges.mkString(",")}, minProfit=${prefs.minProfitThreshold})"
  }
}

/**
 * Exchange selection weights configuration.
 */
case class SelectionWeights(
  profitWeight: Double = 0.4,
  feeWeight: Double = 0.3,
  latencyWeight: Double = 0.2,
  reliabilityWeight: Double = 0.1
) {
  require(
    (profitWeight + feeWeight + latencyWeight + reliabilityWeight - 1.0).abs < 0.001,
    "Selection weights must sum to approximately 1.0"
  )
}

object SelectionWeights {
  implicit val show: Show[SelectionWeights] = Show.show { weights =>
    s"SelectionWeights(profit=${weights.profitWeight}, fee=${weights.feeWeight}, " +
    s"latency=${weights.latencyWeight}, reliability=${weights.reliabilityWeight})"
  }
}
