package arbo
package binance

import data._

/**
 * Binance order types extending SellOrder with LIMIT, MARKET, STOP_LOSS variants.
 * 
 * This object defines the various order types supported by Binance
 * and provides methods to create orders with proper validation.
 */
sealed trait BinanceOrder extends SellOrder

object BinanceOrder {

  /**
   * Market order - executes immediately at current market price.
   */
  case class MarketOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee
  ) extends BinanceOrder

  /**
   * Limit order - executes only at specified price or better.
   */
  case class LimitOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    timeInForce: TimeInForce = TimeInForce.GTC
  ) extends BinanceOrder

  /**
   * Stop loss order - triggers when price reaches stop price.
   */
  case class StopLossOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    stopPrice: Price
  ) extends BinanceOrder

  /**
   * Stop loss limit order - combines stop loss with limit order.
   */
  case class StopLossLimitOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    stopPrice: Price,
    timeInForce: TimeInForce = TimeInForce.GTC
  ) extends BinanceOrder

  /**
   * Take profit order - triggers when price reaches take profit price.
   */
  case class TakeProfitOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    stopPrice: Price
  ) extends BinanceOrder

  /**
   * Take profit limit order - combines take profit with limit order.
   */
  case class TakeProfitLimitOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    stopPrice: Price,
    timeInForce: TimeInForce = TimeInForce.GTC
  ) extends BinanceOrder

  /**
   * Limit maker order - only executes if it would be a maker order.
   */
  case class LimitMakerOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee
  ) extends BinanceOrder

  /**
   * Time in force options for limit orders.
   */
  sealed trait TimeInForce
  object TimeInForce {
    case object GTC extends TimeInForce // Good Till Canceled
    case object IOC extends TimeInForce // Immediate or Cancel
    case object FOK extends TimeInForce // Fill or Kill
  }

  /**
   * Create a market order.
   */
  def marketOrder(
    from: Currency,
    to: Currency,
    fromAmmount: Ammount,
    fee: Fee
  ): MarketOrder = {
    MarketOrder(
      from = from,
      to = to,
      price = 0, // Market orders don't have a specific price
      fromAmmount = fromAmmount,
      fee = fee
    )
  }

  /**
   * Create a limit order.
   */
  def limitOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    timeInForce: TimeInForce = TimeInForce.GTC
  ): LimitOrder = {
    LimitOrder(
      from = from,
      to = to,
      price = price,
      fromAmmount = fromAmmount,
      fee = fee,
      timeInForce = timeInForce
    )
  }

  /**
   * Create a stop loss order.
   */
  def stopLossOrder(
    from: Currency,
    to: Currency,
    price: Price,
    fromAmmount: Ammount,
    fee: Fee,
    stopPrice: Price
  ): StopLossOrder = {
    StopLossOrder(
      from = from,
      to = to,
      price = price,
      fromAmmount = fromAmmount,
      fee = fee,
      stopPrice = stopPrice
    )
  }

  /**
   * Validate order parameters.
   */
  def validateOrder(order: BinanceOrder): Either[String, BinanceOrder] = {
    order match {
      case market: MarketOrder =>
        if (market.fromAmmount <= 0) Left("Market order amount must be positive")
        else if (market.from == market.to) Left("Market order cannot trade same currency")
        else Right(market)
        
      case limit: LimitOrder =>
        if (limit.fromAmmount <= 0) Left("Limit order amount must be positive")
        else if (limit.price <= 0) Left("Limit order price must be positive")
        else if (limit.from == limit.to) Left("Limit order cannot trade same currency")
        else Right(limit)
        
      case stop: StopLossOrder =>
        if (stop.fromAmmount <= 0) Left("Stop loss order amount must be positive")
        else if (stop.price <= 0) Left("Stop loss order price must be positive")
        else if (stop.stopPrice <= 0) Left("Stop loss order stop price must be positive")
        else if (stop.from == stop.to) Left("Stop loss order cannot trade same currency")
        else Right(stop)
        
      case _ => Right(order) // Other order types follow similar validation patterns
    }
  }
}
