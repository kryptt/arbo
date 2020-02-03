package arbo

import higherkindness.droste

import droste.Algebra

import cats.data.NonEmptyList

import data._
import elgot._

object Calculator {

  import SellTree._
  import SellSelection._

  def optionsCoalgebra(sellOptions: GetSellOptions, maxDepth: Depth) =
    ElgotCoalgebra[Either[SellStep, ?], SellTree, SellSeed] {
      case (pOrderOpt, holding, depth) if depth > maxDepth =>
        Left(_ => pOrderOpt.fold[SellSelection](InitialState(holding))(firstOrder))
      case (pOrderOpt, holding, depth) =>
      sellOptions(holding)
        .flatMap {
          order => SellOrder.nextHolding(order).map((Some(order), _, depth + 1))
        } match {
          case Nil =>
            println("coalgNil")
            Left (_ => pOrderOpt.fold[SellSelection](InitialState(holding))(firstOrder))
          case firstChild :: otherChildren =>
            println(s"coalgOpts $depth")
            val pOrder = pOrderOpt.getOrElse(SellOrder.emptyOrder(holding))
            Right(SellNode(pOrder, depth, NonEmptyList(firstChild, otherChildren)))
        }
    }

  def optionsAlgebra(terminalCurrency: Currency, maxDepth: Depth) =
    Algebra[SellTree, SellStep] {
      case SellNode(_, depth, _) if depth >= maxDepth =>
        println("msde1")
        _ => noSale("max search depth exceded")
      case TerminalNode(_, depth) if depth > maxDepth =>
        println("msde2")
        _ => noSale("max search depth exceded")
      case SellNode(order, _, outcomes)  =>
        {
          case _: InitialState =>
            println("snis")
            outcomes.map(step => step(firstOrder(order)))
              .reduceLeft(bestSell(terminalCurrency))
          case SellPath(orders) =>
            println("snsp")
            outcomes.map(step => step(appendOrder(order, orders)))
              .reduceLeft(bestSell(terminalCurrency))
          case other => other
        }
      case TerminalNode(Holding(finalCurrency, finalAmmount), _) =>
        {
          case sp @ SellPath(orders) =>
            println("tnsp")
            val initAmmount = initialAmmount(sp)
            val initCurrency = initialCurrency(sp)
            val profit = finalAmmount - initAmmount
            if (finalCurrency != initCurrency) noSale("ends in a different currency")
            else if (profit <= 0) noSale("no profit")
            else SellPath(orders)
          case _:InitialState =>
            println("tnis")
            noSale("no sales selected")
          case ns:NoSale =>
            println("tnns")
            ns
        }
    }

  def initialSeed(holding: Holding): SellSeed =
    (None, holding, 0)

  def selection(sellOptions: GetSellOptions, terminalCurrency: Currency, maxDepth: Depth)(holding: Holding): SellSelection =
    elgot[SellTree, SellSeed, SellStep](
      optionsAlgebra(terminalCurrency, maxDepth),
      optionsCoalgebra(sellOptions, maxDepth))
      .apply(initialSeed(holding))
      .apply(init(holding))

}
