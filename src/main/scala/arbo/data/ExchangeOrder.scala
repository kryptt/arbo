package arbo
package data

import cats.Eq
import monocle.macros.Lenses

/**
 * Enhanced SellOrder wrapper with exchange metadata for routing information.
 * 
 * This case class wraps a SellOrder with additional metadata about which
 * exchange the order should be executed on, enabling multi-exchange routing.
 */
@Lenses
case class ExchangeOrder[O <: SellOrder](
  order: O,
  exchangeName: String,
  priority: Int = 0,
  estimatedLatency: Option[Long] = None, // in milliseconds
  estimatedFee: Option[Fee] = None
) extends SellOrder {
  // Delegate all SellOrder methods to the wrapped order
  def from: Currency = order.from
  def to: Currency = order.to
  def price: Price = order.price
  def fromAmmount: Ammount = order.fromAmmount
  def fee: Fee = order.fee
}

object ExchangeOrder {

  /**
   * Create an ExchangeOrder from a regular SellOrder.
   */
  def apply[O <: SellOrder](
    order: O,
    exchangeName: String,
    priority: Int = 0
  ): ExchangeOrder[O] = {
    ExchangeOrder(
      order = order,
      exchangeName = exchangeName,
      priority = priority,
      estimatedLatency = None,
      estimatedFee = Some(order.fee)
    )
  }

  /**
   * Create an ExchangeOrder with estimated latency.
   */
  def withLatency[O <: SellOrder](
    order: O,
    exchangeName: String,
    estimatedLatency: Long,
    priority: Int = 0
  ): ExchangeOrder[O] = {
    ExchangeOrder(
      order = order,
      exchangeName = exchangeName,
      priority = priority,
      estimatedLatency = Some(estimatedLatency),
      estimatedFee = Some(order.fee)
    )
  }

  /**
   * Create an ExchangeOrder with custom fee estimation.
   */
  def withCustomFee[O <: SellOrder](
    order: O,
    exchangeName: String,
    estimatedFee: Fee,
    priority: Int = 0
  ): ExchangeOrder[O] = {
    ExchangeOrder(
      order = order,
      exchangeName = exchangeName,
      priority = priority,
      estimatedLatency = None,
      estimatedFee = Some(estimatedFee)
    )
  }

  /**
   * Extract the underlying order from an ExchangeOrder.
   */
  def unwrap[O <: SellOrder](exchangeOrder: ExchangeOrder[O]): O = {
    exchangeOrder.order
  }

  /**
   * Compare ExchangeOrders by priority (higher priority first).
   */
  def compareByPriority[O <: SellOrder](
    a: ExchangeOrder[O],
    b: ExchangeOrder[O]
  ): Int = {
    b.priority.compareTo(a.priority)
  }

  /**
   * Compare ExchangeOrders by estimated latency (lower latency first).
   */
  def compareByLatency[O <: SellOrder](
    a: ExchangeOrder[O],
    b: ExchangeOrder[O]
  ): Int = {
    (a.estimatedLatency, b.estimatedLatency) match {
      case (Some(latA), Some(latB)) => latA.compareTo(latB)
      case (Some(_), None) => -1
      case (None, Some(_)) => 1
      case (None, None) => 0
    }
  }

  /**
   * Compare ExchangeOrders by estimated fee (lower fee first).
   */
  def compareByFee[O <: SellOrder](
    a: ExchangeOrder[O],
    b: ExchangeOrder[O]
  ): Int = {
    (a.estimatedFee, b.estimatedFee) match {
      case (Some(feeA), Some(feeB)) =>
        if (feeA.currency == feeB.currency) {
          feeA.ammount.compareTo(feeB.ammount)
        } else {
          0 // Cannot compare fees in different currencies
        }
      case (Some(_), None) => -1
      case (None, Some(_)) => 1
      case (None, None) => 0
    }
  }

  implicit def exchangeOrderEq[O <: SellOrder]: Eq[ExchangeOrder[O]] = 
    Eq.fromUniversalEquals[ExchangeOrder[O]]
}
