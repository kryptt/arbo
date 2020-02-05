package arbo.data

case class Holding(currency: Currency, ammount: Ammount)

case class Fee(ammount: Ammount, currency: Currency)

case class SellOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)

object SellOrder {

  def toAmmount(order: SellOrder): Option[Ammount] =
    if (order.fee.currency == order.to)
      Some((order.fromAmmount / order.price) - order.fee.ammount)
    else if (order.fee.currency == order.from)
      Some((order.fromAmmount - order.fee.ammount) / order.price)
    else None

  def nextHolding(order: SellOrder): Option[Holding] =
    toAmmount(order).map(Holding(order.to, _))

  def originalHolding(order: SellOrder): Holding = Holding(order.from, order.fromAmmount)

  def emptyOrder(holding: Holding): SellOrder = SellOrder(holding.currency, holding.currency, 1, holding.ammount, Fee(0, holding.currency))

}

