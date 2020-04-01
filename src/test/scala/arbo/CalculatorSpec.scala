package arbo

import cats.Id
import cats.syntax.partialOrder._

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.Matcher
import org.specs2.matcher.MatchResult

import data._
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._

class CalculatorSpec extends Specification with ScalaCheck {
  def is = s2"""
    Works when there are no options ${noOptions()}
    Does a three stop hop ${threeHops()}
    Runs through arbitrary scenarios ${calcProperty}
"""

  import Generators._
  import SellTree._
  import SellSelection._

  val initialHolding = Holding("EUR", 1000)

  def noOptions(): MatchResult[SellSelection[SellOrder]] = {
    val tree = TerminalNode(SellOrder.emptyOrder(initialHolding), 0)
    val result = Calculator.optionsAlgebra
      .run(tree)
      .apply(InitialState(initialHolding))

    result must beEqualTo(noSale("no sales selected"))
  }

  def holdCloseTo(fH: Holding): Matcher[Holding] =
    (beCloseTo(fH.ammount, 8.significantFigures) ^^ { (h: Holding) =>
      h.ammount
    }).and(beEqualTo(fH.currency) ^^ { (h: Holding) =>
      h.currency
    })

  def threeHops(): MatchResult[Holding] = {
    val sellOptions: GetSellOptions[Id, SellOrder] = {
      case Holding("EUR", _) =>
        List(SellOrder("EUR", "BTC", 6500, 1000, Fee(0.12, "EUR")))
      case Holding("BTC", _) =>
        List(SellOrder("BTC", "USD", 0.000125, 0.15382769, Fee(0.15, "USD")))
      case Holding("USD", _) =>
        List(SellOrder("USD", "ETH", 165, 1230.47152, Fee(0.15, "USD")))
      case Holding("ETH", _) =>
        List(SellOrder("ETH", "EUR", 0.006329114, 7.45649416, Fee(0.12, "EUR")))
      case _ => Nil
    }

    val fH = Holding("EUR", 1178.00606314)

    val result = Calculator.selection(sellOptions, "EUR", 4)(initialHolding)
    finalHolding(result.asInstanceOf[SellPath[SellOrder]]).get must holdCloseTo(fH)
  }

  val calculatorGen: Gen[(Holding, Int, SellSelection[SellOrder])] = for {
    holding <- holdingGen
    maxDepth <- depthGen
    result <- Calculator.selection(getSellOptionsGen, holding.currency, maxDepth)(holding)
  } yield (holding, maxDepth, result)

  val calcProperty = Prop
    .forAll(calculatorGen) {
      case (holding, depth, InitialState(h)) =>
        h == holding && depth < 2
      case (holding, depth, sp @ SellPath(os)) =>
        val h = SellSelection.finalHolding(sp)
        os.length <= depth && h.fold(false)(_ >= holding)
      case _ => true

    }
    .set(minTestsOk = 100, workers = 10, maxSize = 6)
}
