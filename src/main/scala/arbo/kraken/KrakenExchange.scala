package arbo
package kraken

import data._
import exchanges.{Exchange, ExchangeHealth, ExchangeStatus}

import cats.data.NonEmptyList
import cats.effect.{Async, Resource}
import cats.syntax.functor._

import org.http4s.client.Client

import java.time.Instant

/**
 * Kraken exchange implementation of the Exchange trait.
 */
class KrakenExchange[F[_]: Async](
  config: Config,
  client: Client[F]
) extends Exchange[F, KrakenOrder] {

  val name: String = "kraken"

  def supportedPairs: F[List[CurrencyPair]] = {
    // Use existing RestClient to get supported pairs
    RestClient.apply[F](config, client).use { restClient =>
      restClient.assetPairs.map(_.keys.toList)
    }
  }

  def getSellOptions(holding: Holding): F[SellOptions[KrakenOrder]] = {
    RestClient.apply[F](config, client).use { restClient =>
      restClient.assetPairs.flatMap { fees =>
        val candidates = fees.filter {
          case (k, _) => CurrencyPair.holds(k, holding)
        }
        
        if (candidates.isEmpty) {
          Async[F].pure(List.empty[KrakenOrder])
        } else {
          val pairs = NonEmptyList.fromList(candidates.keys.toList).get
          restClient.ticker(pairs).map { rates =>
            rates.toList.flatMap {
              case (cp @ CurrencyPair(base, cvar), ticker) if base == holding.currency =>
                val opts = fees(cp)
                Some(Order.variableOrder(
                  opts,
                  fee = feeAmount(opts.maker),
                  basePrice = (ticker.bid + ticker.ask) / 2,
                  holding = holding,
                  to = cvar
                ))
              case (cp @ CurrencyPair(base, cvar), ticker) if cvar == holding.currency =>
                val opts = fees(cp)
                Some(Order.baseOrder(
                  opts,
                  fee = feeAmount(opts.maker),
                  basePrice = (ticker.bid + ticker.ask) / 2,
                  holding = holding,
                  to = base
                ))
              case _ => None
            }.flatten
          }
        }
      }
    }
  }

  def execute(order: KrakenOrder): F[Holding] = {
    RestClient.apply[F](config, client).use { restClient =>
      restClient.execute(order)
    }
  }

  def healthCheck: F[ExchangeHealth] = {
    val startTime = Instant.now()
    
    RestClient.apply[F](config, client).use { restClient =>
      restClient.assetPairs.map { _ =>
        val latency = java.time.Duration.between(startTime, Instant.now()).toMillis
        ExchangeHealth(
          exchangeName = name,
          status = ExchangeStatus.Healthy,
          latency = Some(latency),
          errorRate = None,
          lastChecked = Instant.now()
        )
      }
    }.handleError { _ =>
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
    RestClient.apply[F](config, client).use { restClient =>
      restClient.ticker(pairs)
    }
  }

  def assetPairs: F[AssetPairsInfo] = {
    RestClient.apply[F](config, client).use { restClient =>
      restClient.assetPairs
    }
  }

  private def feeAmount(makerFees: List[FeeOption]): Ammount = {
    val userVolume = 100000
    makerFees
      .findLast(_.volume < userVolume)
      .fold[Ammount](0.0024)(_.percentage / 100)
  }
}

object KrakenExchange {
  def apply[F[_]: Async](
    config: Config,
    client: Client[F]
  ): Resource[F, KrakenExchange[F]] = {
    Resource.pure(new KrakenExchange(config, client))
  }
}
