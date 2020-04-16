package arbo
package data

import cats.kernel.PartialOrder
import cats.Eq

import monocle.macros.Lenses

import scala.util.Try
import scala.math.BigDecimal.RoundingMode.FLOOR

@Lenses
case class Holding(currency: Currency, ammount: Ammount) extends Serializable

object Holding {

  implicit val holdingPartialOrder = new PartialOrder[Holding] {
    def partialCompare(a: Holding, b: Holding): Double =
      if (a.currency == b.currency) (a.ammount.toDouble - b.ammount.toDouble)
      else Double.NaN
  }

}

case class Fee(ammount: Ammount, currency: Currency) extends Serializable

@Lenses
case class SellOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends Serializable

object SellOrder {

  def toAmmount(order: SellOrder): Option[Ammount] = Try {
    if (order.fee.currency == order.to)
      ((order.fromAmmount / order.price) - order.fee.ammount)
    else if (order.fee.currency == order.from)
      ((order.fromAmmount - order.fee.ammount) / order.price)
    else ???
  }
    .map(_.setScale(order.fromAmmount.scale, FLOOR))
    .filter(_ > 0)
    .toOption

  def nextHolding(order: SellOrder): Option[Holding] =
    toAmmount(order).map(Holding(order.to, _))

  def originalHolding(order: SellOrder): Holding =
    Holding(order.from, order.fromAmmount)

  def emptyOrder(holding: Holding): SellOrder =
    SellOrder(holding.currency, holding.currency, 1, holding.ammount, Fee(0, holding.currency))

  implicit val sellOrderEq: Eq[SellOrder] = Eq.fromUniversalEquals[SellOrder]

}
