package arbo
package kraken

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Prop

class OrderSpec extends Specification with ScalaCheck {
  def is = s2"""
    correctly formats order price and volume $order
"""

  import Generators._

  def order = Prop.forAll(orderGen) {
    case (order, pairDecimals, lotDecimals) =>
      decimalsCheck(order.krakenPrice, "Price", pairDecimals)
        .and(decimalsCheck(order.krakenVolume, "Volume", lotDecimals))
  }

  def decimalsCheck(decimal: String, label: String, maxDecimals: Int) =
    decimal.must(beMatching(decimalsRegEx(maxDecimals)))
      .setMessage(s"$label does not match ($decimal <- $maxDecimals)")

  def decimalsRegEx(maxDecimals: Int): String =
    if (maxDecimals == 0) "^\\d+$$"
    else s"^\\d+(\\.\\d{1,${maxDecimals})?$$"

}
