package arbo
package kraken

import data._

import BigDecimal.RoundingMode.{DOWN => RoundDown}

sealed trait KrakenOrder extends SellOrder {
  def krakenPair: String
  def krakenPrice: Price
  def krakenVolume: Ammount
  def krakenType: String
}

class BaseOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee, pairDecimals: Int, lotDecimals: Int)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = to + from
  def krakenPrice = price.setScale(pairDecimals, RoundDown)
  def krakenVolume = SellOrder.toAmmount(this)
    .getOrElse(BigDecimal(0))
    .setScale(lotDecimals, RoundDown)
  def krakenType = "buy"
}
class VariableOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee, pairDecimals: Int, lotDecimals: Int)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = from + to
  def krakenPrice = (1 / price).setScale(pairDecimals, RoundDown)
  def krakenVolume = fromAmmount.setScale(lotDecimals, RoundDown)
  def krakenType = "sell"
}

object Order {

  def baseSell(so: SellOrder): BaseOrder =
    new BaseOrder(so.from, so.to, so.price, so.fromAmmount, so.fee, 8, 8)

  def varSell(so: SellOrder): VariableOrder =
    new VariableOrder(so.from, so.to, so.price, so.fromAmmount, so.fee, 8, 8)

  def baseOrder(
    opts: AssetPairOptions,
    fee: Ammount,
    basePrice: Price,
    holding: Holding,
    to: Currency): BaseOrder = {
    val from = holding.currency
    val ammount = holding.ammount / (1 + fee)
    val feeAmmount = ammount * fee
    new BaseOrder(from, to,
                  (1 / basePrice), ammount,
                  Fee(feeAmmount, from),
                  opts.pairDecimals, opts.lotDecimals)
  }

  def variableOrder(
    opts: AssetPairOptions,
      fee: Ammount,
      basePrice: Price,
      holding: Holding,
      to: Currency): VariableOrder = {
    val from = holding.currency
    val ammount = holding.ammount / (1 + fee)
    val feeAmmount = ammount * fee
    new VariableOrder(from, to,
                      basePrice, ammount,
                      Fee(feeAmmount, from),
                      opts.pairDecimals, opts.lotDecimals)
  }
}
