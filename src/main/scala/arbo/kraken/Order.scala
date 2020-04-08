package arbo
package kraken

import data._

sealed trait KrakenOrder extends SellOrder {
  def krakenPair: String
  def krakenPrice: String
  def krakenVolume: String
  def krakenType: String

  @inline
  protected final def krakenAmmount(n: Ammount): Ammount =
    n.setScale(6, BigDecimal.RoundingMode.DOWN)
}
class BaseOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = to + from
  def krakenPrice = krakenAmmount(price).toString
  def krakenVolume = krakenAmmount(SellOrder.toAmmount(this).getOrElse(0)).toString()
  def krakenType = "buy"
}
class VariableOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = from + to
  def krakenPrice = krakenAmmount(1 / price).toString
  def krakenVolume = krakenAmmount(fromAmmount).toString
  def krakenType = "sell"
}

object Order {
  def baseOrder(fee: Ammount, basePrice: Price, holding: Holding, to: Currency): BaseOrder = {
    val from = holding.currency
    val ammount = holding.ammount / (1 + fee)
    val feeAmmount = ammount * fee
    new BaseOrder(from, to, 1 / basePrice, ammount, Fee(feeAmmount, from))
  }

  def variableOrder(
      fee: Ammount,
      basePrice: Price,
      holding: Holding,
      to: Currency): VariableOrder = {
    val from = holding.currency
    val ammount = holding.ammount / (1 + fee)
    val feeAmmount = ammount * fee
    new VariableOrder(from, to, basePrice, ammount, Fee(feeAmmount, from))
  }
}
