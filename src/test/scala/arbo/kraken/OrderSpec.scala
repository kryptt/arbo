package arbo
package kraken

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Prop
import scala.util.matching.Regex

class OrderSpec extends Specification with ScalaCheck {
  def is = s2"""
    correctly formats order price and volume $order
"""

  import Generators._

  def order = Prop.forAllNoShrink(orderGen) {
    case (order, pairDecimals, lotDecimals) =>
      decimalsCheck(order.krakenPrice, "Price", pairDecimals)
        .and(decimalsCheck(order.krakenVolume, "Volume", lotDecimals))
  }

  def decimalsCheck(decimal: String, label: String, maxDecimals: Int) =
    decimal.must(beMatching(decimalsRegEx(maxDecimals)))
      .setMessage(s"$label does not match ($decimal <- $maxDecimals)")

  def decimalsRegEx(maxDecimals: Int): Regex =
    if (maxDecimals == 0) new Regex("^-?\\d+(\\.0)?$$")
    else new Regex(s"^-?\\d+(\\.\\d{1,${maxDecimals}})?$$")

}
