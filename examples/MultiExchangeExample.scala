package arbo.examples

import arbo._
import arbo.data._
import arbo.exchanges._
import arbo.kraken.KrakenExchange
import arbo.binance.BinanceRestClient

import cats.effect.{IO, IOApp, ExitCode}
import cats.syntax.functor._
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global

/**
 * Example of how to use the multi-exchange system with both Kraken and Binance.
 */
object MultiExchangeExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val exampleHolding = Holding("BTC", BigDecimal("0.1"))
    
    BlazeClientBuilder[IO](global).resource.use { httpClient =>
      for {
        // Load configuration
        exchangeConfig <- ExchangeConfig.load[IO].value
        
        // Create multi-exchange manager
        manager = MultiExchangeManager[IO](exchangeConfig, httpClient)
        
        // Create registry with all available exchanges
        registry <- manager.createRegistry
        
        // Get all exchanges
        exchanges <- registry.getExchanges
        
        // Check health of all exchanges
        healthStatus <- registry.getHealthStatus
        
        // Get healthy exchanges
        healthyExchanges <- registry.getHealthyExchanges
        
        // Example: Find sell options for a holding
        _ <- if (healthyExchanges.nonEmpty) {
          val firstExchange = healthyExchanges.head
          for {
            sellOptions <- firstExchange.getSellOptions(exampleHolding)
            _ <- IO(println(s"Found ${sellOptions.length} sell options on ${firstExchange.name}"))
          } yield ()
        } else {
          IO(println("No healthy exchanges available"))
        }
        
        // Example: Use multi-exchange selection
        _ <- if (healthyExchanges.length >= 2) {
          val exchangesNel = NonEmptyList.fromList(healthyExchanges).get
          Calculator.multiExchangeSelection(
            exchanges = exchangesNel,
            terminalCurrency = "EUR",
            maxDepth = 3
          )(exampleHolding).flatMap { result =>
            result match {
              case MultiExchangePath.ExchangePath(orders, profit, fees, exchanges) =>
                IO(println(s"Multi-exchange path found: ${orders.length} orders, profit: $profit, fees: $fees, exchanges: ${exchanges.mkString(",")}"))
              case MultiExchangePath.NoSale(reasons) =>
                IO(println(s"No sale possible: ${reasons.mkString_(", ")}"))
              case MultiExchangePath.InitialState(holding) =>
                IO(println(s"Initial state: $holding"))
            }
          }
        } else {
          IO(println("Need at least 2 exchanges for multi-exchange selection"))
        }
        
      } yield ExitCode.Success
    }
  }
}
