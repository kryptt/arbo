package arbo

import org.specs2.matcher.MatchResult

import data._

class CalculatorSpec extends org.specs2.mutable.Specification {
  "Calculator" >> {
    "works when there are no options" >> {
      noOptions()
    }
    "does a three stop hop" >> {
      threeHops()
    }
  }

  import SellTree._
  import SellSelection._

  val initialHolding = Holding("EUR", 1000)

  def noOptions(): MatchResult[SellSelection] = {
    val tree = TerminalNode(initialHolding, 0)
    val result = Calculator.optionsAlgebra("EUR", 6)
      .run(tree)
      .apply(InitialState(initialHolding))

    result must beEqualTo(noSale("no sales selected"))
  }

  def threeHops(): MatchResult[Holding] = {
    val sellOptions: GetSellOptions = {
      case Holding("EUR", _) =>
        List(SellOrder("EUR", "BTC", 6500, 1000, Fee(0.12, "EUR")))
      case Holding("BTC", _) =>
        List(SellOrder("BTC", "USD", 0.000125, 0.153827692, Fee(0.15, "USD")))
      case Holding("USD", _) =>
          List(SellOrder("USD", "ETH", 165, 1230.471536, Fee(0.15, "USD")))
      case Holding("ETH", _) =>
        List(SellOrder("ETH", "EUR", 0.006329114, 7.456494158, Fee(0.12, "EUR")))
      case _ => Nil

    }

    val fH = Holding("EUR", 1178.006985862)

    val result = Calculator.selection(sellOptions, "EUR", 6)(initialHolding)

    finalHolding(result.asInstanceOf[SellPath]).get must beEqualTo(fH)
  }
}
