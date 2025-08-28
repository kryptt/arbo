package arbo
package data

import cats.data.NonEmptyList
import cats.Eq
import monocle.Lens
import monocle.function.all._

/**
 * Enhanced SellSelection with exchange routing and profit tracking across exchanges.
 * 
 * This extends the existing SellSelection to support multi-exchange trading paths,
 * where different parts of a trading sequence can be executed on different exchanges.
 */
sealed trait MultiExchangePath[+O <: SellOrder] extends Any with Serializable

object MultiExchangePath {
  
  case class InitialState(holding: Holding) extends AnyVal with MultiExchangePath[Nothing]

  case class ExchangePath[+O <: SellOrder](
    orders: SellSequence[ExchangeOrder[O]],
    totalProfit: Ammount,
    totalFees: Ammount,
    exchangesUsed: Set[String]
  ) extends AnyVal with MultiExchangePath[O]

  case class NoSale(reasons: NonEmptyList[String]) extends AnyVal with MultiExchangePath[Nothing]

  object ExchangePath {
    def orders[O <: SellOrder]: Lens[ExchangePath[O], SellSequence[ExchangeOrder[O]]] =
      Lens[ExchangePath[O], SellSequence[ExchangeOrder[O]]](_.orders)(os => ep => ep.copy(orders = os))

    def lastOrder[O <: SellOrder]: Lens[ExchangePath[O], ExchangeOrder[O]] =
      orders[O].composeLens(last)
  }

  def init(holding: Holding): InitialState =
    InitialState(holding)

  def initialCurrency[O <: SellOrder](ep: ExchangePath[O]): Currency =
    ep.orders.head.from

  def initialAmmount[O <: SellOrder](ep: ExchangePath[O]): Ammount =
    ep.orders.head.fromAmmount

  def finalCurrency[O <: SellOrder](ep: ExchangePath[O]): Currency =
    ep.orders.last.to

  def finalAmmount[O <: SellOrder](ep: ExchangePath[O]): Option[Ammount] =
    SellOrder.toAmmount(ep.orders.last)

  def finalHolding[O <: SellOrder](ep: ExchangePath[O]): Option[Holding] =
    finalAmmount(ep).map(Holding(finalCurrency(ep), _))

  def noSale(reason: String): NoSale =
    NoSale(NonEmptyList.one(reason))

  def firstOrder[O <: SellOrder](order: ExchangeOrder[O]): ExchangePath[O] =
    ExchangePath(
      orders = NonEmptyList.one(order),
      totalProfit = 0,
      totalFees = order.fee.ammount,
      exchangesUsed = Set(order.exchangeName)
    )

  def orders[O <: SellOrder](os: NonEmptyList[ExchangeOrder[O]]): ExchangePath[O] = {
    val totalFees = os.toList.map(_.fee.ammount).sum
    val exchangesUsed = os.toList.map(_.exchangeName).toSet
    
    ExchangePath(
      orders = os,
      totalProfit = 0, // Would be calculated based on actual execution
      totalFees = totalFees,
      exchangesUsed = exchangesUsed
    )
  }

  def appendToOrders[O <: SellOrder](order: ExchangeOrder[O], orders: NonEmptyList[ExchangeOrder[O]]): ExchangePath[O] = {
    val newOrders = orders :+ order
    val totalFees = newOrders.toList.map(_.fee.ammount).sum
    val exchangesUsed = newOrders.toList.map(_.exchangeName).toSet
    
    ExchangePath(
      orders = newOrders,
      totalProfit = 0, // Would be calculated based on actual execution
      totalFees = totalFees,
      exchangesUsed = exchangesUsed
    )
  }

  /**
   * Calculate the best multi-exchange path considering profit, fees, and exchange diversity.
   */
  def bestMultiExchangePath[O <: SellOrder](
    left: MultiExchangePath[O],
    right: MultiExchangePath[O]
  ): MultiExchangePath[O] = {

    @inline def whenSameCurrency(left: ExchangePath[O], right: ExchangePath[O]): MultiExchangePath[O] =
      (finalAmmount(left), finalAmmount(right)) match {
        case (None, None) => noSale("incomplete multiExchangePath")
        case (Some(_), None) => left
        case (None, Some(_)) => right
        case (Some(lA), Some(rA)) =>
          val leftNetProfit = lA - left.totalFees
          val rightNetProfit = rA - right.totalFees
          val diff = rightNetProfit - leftNetProfit
          
          if (diff > 0) right
          else if (diff < 0) left
          else {
            // If profits are equal, prefer path with fewer exchanges (simpler execution)
            if (left.exchangesUsed.size < right.exchangesUsed.size) left
            else if (right.exchangesUsed.size < left.exchangesUsed.size) right
            else if (left.orders.length > right.orders.length) right
            else left
          }
      }

    @inline def whenExchangePath(left: ExchangePath[O], right: ExchangePath[O]): MultiExchangePath[O] =
      if (finalCurrency(left) != finalCurrency(right))
        noSale("unexpected finalCurrency mismatch in multi-exchange path")
      else whenSameCurrency(left, right)

    (left, right) match {
      case (left: ExchangePath[O], right: ExchangePath[O]) => whenExchangePath(left, right)
      case (left: ExchangePath[O], _) => left
      case (_, right: ExchangePath[O]) => right
      case (left, _: InitialState) => left
      case (_: InitialState, right) => right
      case (NoSale(lReasons), NoSale(rReasons)) =>
        NoSale(NonEmptyList(lReasons.head, List(rReasons.head)))
    }
  }

  /**
   * Get the primary exchange used in this path (the exchange with the most orders).
   */
  def primaryExchange[O <: SellOrder](ep: ExchangePath[O]): String = {
    val exchangeCounts = ep.orders.toList.groupBy(_.exchangeName).mapValues(_.length)
    exchangeCounts.maxBy(_._2)._1
  }

  /**
   * Get the exchange distribution for this path.
   */
  def exchangeDistribution[O <: SellOrder](ep: ExchangePath[O]): Map[String, Int] = {
    ep.orders.toList.groupBy(_.exchangeName).mapValues(_.length)
  }

  /**
   * Check if this path uses multiple exchanges.
   */
  def isMultiExchange[O <: SellOrder](ep: ExchangePath[O]): Boolean = {
    ep.exchangesUsed.size > 1
  }

  /**
   * Calculate the average latency for this path.
   */
  def averageLatency[O <: SellOrder](ep: ExchangePath[O]): Option[Long] = {
    val latencies = ep.orders.toList.flatMap(_.estimatedLatency)
    if (latencies.nonEmpty) {
      Some(latencies.sum / latencies.length)
    } else {
      None
    }
  }

  implicit val eqMultiExchangePath: Eq[MultiExchangePath[SellOrder]] = Eq.instance {
    case (_: NoSale, _: NoSale) => true
    case (InitialState(a), InitialState(b)) => a == b
    case (ExchangePath(as, profitA, feesA, exchangesA), ExchangePath(bs, profitB, feesB, exchangesB)) => 
      as == bs && profitA == profitB && feesA == feesB && exchangesA == exchangesB
    case _ => false
  }
}
