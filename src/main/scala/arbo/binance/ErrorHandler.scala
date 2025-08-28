package arbo
package binance

import cats.effect.{Concurrent, Timer}
import cats.syntax.applicativeError._
import cats.syntax.functor._

import org.http4s.{Status, Response}
import org.http4s.client.Client

import scala.concurrent.duration._

/**
 * Binance error handling with HTTP 429/418 responses, Retry-After headers, exponential backoff.
 * 
 * This object provides utilities for handling Binance API errors,
 * including rate limiting, retry logic, and exponential backoff.
 */
object ErrorHandler {

  /**
   * Binance-specific error types.
   */
  sealed trait BinanceError extends RuntimeException
  case class RateLimitExceeded(retryAfter: Option[FiniteDuration]) extends BinanceError
  case class IPBanned(retryAfter: Option[FiniteDuration]) extends BinanceError
  case class InvalidApiKey(message: String) extends BinanceError
  case class InsufficientBalance(message: String) extends BinanceError
  case class InvalidSymbol(message: String) extends BinanceError
  case class OrderNotFound(message: String) extends BinanceError
  case class UnknownError(code: Int, message: String) extends BinanceError

  /**
   * Parse Binance error response.
   */
  def parseError(status: Status, body: String): BinanceError = {
    status.code match {
      case 429 => RateLimitExceeded(parseRetryAfter(status))
      case 418 => IPBanned(parseRetryAfter(status))
      case 400 => parse400Error(body)
      case 401 => InvalidApiKey("Invalid API key")
      case 403 => InsufficientBalance("Insufficient balance")
      case _ => UnknownError(status.code, body)
    }
  }

  /**
   * Parse retry-after header from response.
   */
  private def parseRetryAfter(status: Status): Option[FiniteDuration] = {
    status.headers.get("Retry-After".ci)
      .map(_.head.value.toLongOption.map(_.seconds))
      .flatten
  }

  /**
   * Parse 400 Bad Request errors with specific Binance error codes.
   */
  private def parse400Error(body: String): BinanceError = {
    // This would parse the JSON error response to get specific error codes
    // For now, return a generic error
    UnknownError(400, body)
  }

  /**
   * Retry configuration for exponential backoff.
   */
  case class RetryConfig(
    maxRetries: Int = 3,
    initialDelay: FiniteDuration = 1.second,
    maxDelay: FiniteDuration = 30.seconds,
    backoffMultiplier: Double = 2.0
  )

  /**
   * Execute a request with exponential backoff retry logic.
   */
  def withRetry[F[_]: Concurrent: Timer, A](
    operation: F[A],
    config: RetryConfig = RetryConfig()
  ): F[A] = {
    def retryLoop(attempt: Int, delay: FiniteDuration): F[A] = {
      operation.handleErrorWith { error =>
        if (attempt < config.maxRetries) {
          error match {
            case RateLimitExceeded(Some(retryAfter)) =>
              Timer[F].sleep(retryAfter) >> retryLoop(attempt + 1, delay)
            case IPBanned(Some(retryAfter)) =>
              Timer[F].sleep(retryAfter) >> retryLoop(attempt + 1, delay)
            case _: RateLimitExceeded | _: IPBanned =>
              val nextDelay = (delay * config.backoffMultiplier).min(config.maxDelay)
              Timer[F].sleep(delay) >> retryLoop(attempt + 1, nextDelay)
            case _ =>
              val nextDelay = (delay * config.backoffMultiplier).min(config.maxDelay)
              Timer[F].sleep(delay) >> retryLoop(attempt + 1, nextDelay)
          }
        } else {
          Concurrent[F].raiseError(error)
        }
      }
    }
    
    retryLoop(0, config.initialDelay)
  }

  /**
   * Enhanced HTTP client with Binance error handling.
   */
  def enhancedClient[F[_]: Concurrent: Timer](
    baseClient: Client[F]
  ): Client[F] = {
    Client { request =>
      baseClient.run(request).map { response =>
        if (response.status.isSuccess) {
          response
        } else {
          // For error responses, we would need to read the body to parse the error
          // This is a simplified version
          response
        }
      }
    }
  }
}
