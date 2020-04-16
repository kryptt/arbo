package arbo
package kraken

import data._

import BigDecimal.RoundingMode.FLOOR
import scala.util.Try

sealed trait KrakenOrder extends SellOrder {
  def krakenPair: String
  def krakenPrice: String
  def krakenVolume: String
  def krakenType: String
}

class BaseOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = to + from
  def krakenPrice = price.underlying.toPlainString
  def krakenVolume = SellOrder.toAmmount(this).getOrElse(BigDecimal(0)).underlying.toPlainString
  def krakenType = "buy"
  override def toString() = s"BaseOrder($from, $to, $krakenType, $krakenPrice, $krakenVolume)"
}

class VariableOrder(from: Currency, to: Currency, price: Price, fromAmmount: Ammount, fee: Fee)
    extends SellOrder(from, to, price, fromAmmount, fee)
    with KrakenOrder {
  def krakenPair = from + to
  def krakenPrice = Try(1 / price).getOrElse(BigDecimal(0)).setScale(price.scale, FLOOR).underlying.toPlainString
  def krakenVolume = fromAmmount.underlying.toPlainString
  def krakenType = "sell"
  override def toString() = s"VariableOrder($from, $to, $krakenType, $krakenPrice, $krakenVolume)"
}

object Order {

  def baseSell(so: SellOrder, pairDecimals: Int, lotDecimals: Int): BaseOrder =
    new BaseOrder(so.from, so.to, so.price.setScale(pairDecimals, FLOOR), so.fromAmmount.setScale(lotDecimals, FLOOR), so.fee)

  def varSell(so: SellOrder, pairDecimals: Int, lotDecimals: Int): VariableOrder =
    new VariableOrder(so.from, so.to, so.price.setScale(pairDecimals, FLOOR), so.fromAmmount.setScale(lotDecimals, FLOOR), so.fee)

  def baseOrder(
    opts: AssetPairOptions,
    fee: Ammount,
    basePrice: Price,
    holding: Holding,
    to: Currency): BaseOrder = {
    val from = holding.currency
    val ammount = (holding.ammount / (1 + fee)).setScale(opts.lotDecimals, FLOOR)
    val feeAmmount = (ammount * fee).setScale(opts.lotDecimals, FLOOR)
    new BaseOrder(from, to,
                  (1 / basePrice).setScale(opts.pairDecimals, FLOOR), ammount,
                  Fee(feeAmmount, from))
  }

  def variableOrder(
    opts: AssetPairOptions,
      fee: Ammount,
      basePrice: Price,
      holding: Holding,
      to: Currency): VariableOrder = {
    val from = holding.currency
    val ammount = (holding.ammount / (1 + fee)).setScale(opts.lotDecimals, FLOOR)
    val feeAmmount = (ammount * fee).setScale(opts.lotDecimals, FLOOR)
    new VariableOrder(from, to,
                      basePrice.setScale(opts.pairDecimals, FLOOR), ammount,
                      Fee(feeAmmount, from))
  }
}
