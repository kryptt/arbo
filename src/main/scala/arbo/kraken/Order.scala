package arbo
package kraken

import data._

sealed trait KrakenOrder extends SellOrder {
  def krakenPair: String
  def krakenPrice: String
  def krakenVolume: String
  def krakenType: String
}

class BaseOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee, pairDecimals: Int, lotDecimals: Int)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = to + from
  def krakenPrice = s"%.${pairDecimals}f".format(price)
  def krakenVolume = s"%.${lotDecimals}f".format(SellOrder.toAmmount(this).getOrElse(BigDecimal(0)))
  def krakenType = "buy"
  override def toString() = s"BaseOrder($from, $to, ${price.underlying.toPlainString()}, ${fromAmmount.underlying.toPlainString()})"
}
class VariableOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee, pairDecimals: Int, lotDecimals: Int)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = from + to
  def krakenPrice = s"%.${pairDecimals}f".format(1 / price)
  def krakenVolume = s"%.${lotDecimals}f".format(fromAmmount)
  def krakenType = "sell"
  override def toString() = s"VariableOrder($from, $to, ${price.underlying.toPlainString()}, ${fromAmmount.underlying.toPlainString()})"
}

object Order {

  def baseSell(so: SellOrder, pairDecimals: Int, lotDecimals: Int): BaseOrder =
    new BaseOrder(so.from, so.to, so.price, so.fromAmmount, so.fee, pairDecimals, lotDecimals)

  def varSell(so: SellOrder, pairDecimals: Int, lotDecimals: Int): VariableOrder =
    new VariableOrder(so.from, so.to, so.price, so.fromAmmount, so.fee, pairDecimals, lotDecimals)

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
