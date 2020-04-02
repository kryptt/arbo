package arbo
package kraken

import org.scalacheck.Gen

object Generators {

  import data.Generators._

  val baseOrderGen: Gen[BaseOrder] =
    sellOrderGen.map(Order.baseSell)

  val varOrderGen: Gen[VariableOrder] =
    sellOrderGen.map(Order.varSell)

  val orderGen: Gen[KrakenOrder] =
    Gen.oneOf(varOrderGen, baseOrderGen)

}
