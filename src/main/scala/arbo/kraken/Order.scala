package arbo
package kraken

import data._

sealed trait KrakenOrder extends SellOrder {
  def krakenPair: String
  def krakenPrice: String
}
class BaseOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = from + to
  def krakenPrice = price.toString
}
class VariableOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = to + from
  def krakenPrice = (1 / price).toString
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
