package arbo
package kraken

import org.scalacheck.Gen

object Generators {

  import data.Generators._

  val orderDecimals = Gen.choose(0, 8)

  def baseOrderGen(pairDecimals: Int, lotDecimals: Int): Gen[BaseOrder] =
    sellOrderGen.map(Order.baseSell(_, pairDecimals, lotDecimals))

  def varOrderGen(pairDecimals: Int, lotDecimals: Int): Gen[VariableOrder] =
    sellOrderGen.map(Order.varSell(_, pairDecimals, lotDecimals))

  val orderGen: Gen[(KrakenOrder, Int, Int)] = for {
    pD <- orderDecimals
    lD <- orderDecimals
    ko <- Gen.oneOf(varOrderGen(pD, lD), baseOrderGen(pD, lD))
  } yield (ko, pD, lD)

}
