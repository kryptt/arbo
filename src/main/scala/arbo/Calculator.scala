package arbo

import higherkindness.droste

import droste.Algebra

import cats.data.NonEmptyList

import data._
import elgot._

object Calculator {

  import SellTree._
  import SellSelection._

  def optionsCoalgebra(sellOptions: GetSellOptions, terminalCurrency: Currency, maxDepth: Depth) = {
    ElgotCoalgebra[Either[SellStep, ?], SellTree, SellSeed] {
      case (_, _, depth) if depth > maxDepth =>
        Left(_ => noSale(s"maximum search depth ($maxDepth) exceeded"))
      case (Some(pOrder), holding, depth) if holding.currency == terminalCurrency =>
        Right(TerminalNode(pOrder, depth))
      case (pOrderOpt, holding, depth) =>
        val pOrder = ensurePreviousOrder(pOrderOpt, holding)
        sellOptions(holding)
          .flatMap {
            order => SellOrder.nextHolding(order).map((Some(order), _, depth + 1))
          } match {
            case Nil =>
              Right(TerminalNode(pOrder, depth))
            case firstChild :: otherChildren =>
              Right(SellNode(pOrder, depth, NonEmptyList(firstChild, otherChildren)))
          }
    }
  }

  val optionsAlgebra = Algebra[SellTree, SellStep] {
    case SellNode(order, _, outcomes)  =>
      {
        case SellPath(orders) =>
          selectBestOutcome(outcomes, appendToOrders(order, orders))
        case _:InitialState =>
          selectBestOutcome(outcomes, firstOrder(order))
        case ns:NoSale => ns
      }
    case TerminalNode(lastOrder, _) =>
      {
        case sp @ SellPath(orders) =>
          val initAmmount = initialAmmount(sp)
          val initCurrency = initialCurrency(sp)
          val finAmmount = SellOrder.toAmmount(lastOrder)
          val finCurrency = lastOrder.to
          val profit = finAmmount.fold(BigDecimal(-1))(_ - initAmmount)
          if (finCurrency != initCurrency) noSale("ends in a different currency")
          else if (profit <= 0) noSale(s"no profit ($profit)")
          else SellPath(orders :+ lastOrder)
        case _:InitialState => noSale("no sales selected")
        case ns:NoSale => ns
      }
  }


  def selection(sellOptions: GetSellOptions, terminalCurrency: Currency, maxDepth: Depth)(holding: Holding): SellSelection =
    elgot[SellTree, SellSeed, SellStep](
      optionsAlgebra,
      optionsCoalgebra(sellOptions, terminalCurrency, maxDepth))
      .apply(initialSeed(holding))
      .apply(init(holding))

  @inline def initialSeed(holding: Holding): SellSeed =
    (None, holding, 0)

  @inline def ensurePreviousOrder(pOrderOpt: Option[SellOrder], holding: Holding) =
    pOrderOpt.getOrElse(SellOrder.emptyOrder(holding))

  @inline def selectBestOutcome(outcomes: NonEmptyList[SellStep], sel: SellSelection) =
    outcomes.map(step => step(sel))
      .reduceLeft(bestSell)

}
