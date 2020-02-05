package arbo

import higherkindness.droste

import droste.Algebra

import cats.data.NonEmptyList

import data._
import elgot._

object Calculator {

  import SellTree._
  import SellSelection._

  def optionsCoalgebra(sellOptions: GetSellOptions, terminalCurrency: Currency, maxDepth: Depth): ElgotCoalgebra[Either[SellStep, *], SellTree, SellSeed] = {
    ElgotCoalgebra[Either[SellStep, *], SellTree, SellSeed] {
      case (_, _, depth) if depth > maxDepth =>
        Left(_ => noSale(s"maximum search depth ($maxDepth) exceeded"))
      case (Some(pOrder), holding, depth) if holding.currency == terminalCurrency =>
        Right(TerminalNode(pOrder, depth))
      case (pOrderOpt, holding, depth) =>
        buildChildren(sellOptions, pOrderOpt, holding, depth)
    }
  }

  def optionsAlgebra: Algebra[SellTree, SellStep] =
    Algebra[SellTree, SellStep] {
      case SellNode(order, _, outcomes) =>
        sellNodeStep(order, outcomes)
      case TerminalNode(lastOrder, _) =>
        terminalNodeStep(lastOrder)
    }

  def selection(sellOptions: GetSellOptions, terminalCurrency: Currency, maxDepth: Depth)(holding: Holding): SellSelection =
    elgot[SellTree, SellSeed, SellStep](
      optionsAlgebra,
      optionsCoalgebra(sellOptions, terminalCurrency, maxDepth))
      .apply(initialSeed(holding))
      .apply(init(holding))

  @inline def initialSeed(holding: Holding): SellSeed =
    (None, holding, 0)

  @inline def nextSeed(depth: Depth): SellOrder => Option[SellSeed] =
    (order: SellOrder) => SellOrder.nextHolding(order).map((Some(order), _, depth + 1))

  @inline def buildChildren[L](sellOptions: GetSellOptions, pOrderOpt: Option[SellOrder], holding: Holding, depth: Depth): Either[L, SellTree[SellSeed]] = {
    val pOrder = ensurePreviousOrder(pOrderOpt, holding)
    sellOptions(holding).flatMap(nextSeed(depth)) match {
      case Nil => Right(TerminalNode(pOrder, depth))
      case firstChild :: otherChildren =>
        Right(SellNode(pOrder, depth, NonEmptyList(firstChild, otherChildren)))
    }
  }

  @inline def ensurePreviousOrder(pOrderOpt: Option[SellOrder], holding: Holding): SellOrder =
    pOrderOpt.getOrElse(SellOrder.emptyOrder(holding))

  @inline def terminalNodeStep(lastOrder: SellOrder): SellSelection => SellSelection = {
    case sp@SellPath(orders) =>
      lastSelection(lastOrder, sp, orders)
    case _: InitialState => noSale("no sales selected")
    case ns: NoSale => ns
  }

  @inline def sellNodeStep(order: SellOrder, outcomes: NonEmptyList[SellStep]): SellSelection => SellSelection = {
    case SellPath(orders) =>
      selectBestOutcome(outcomes, appendToOrders(order, orders))
    case _: InitialState =>
      selectBestOutcome(outcomes, firstOrder(order))
    case ns: NoSale => ns
  }

  @inline def lastSelection(lastOrder: SellOrder, sp: SellPath, orders: SellSequence): SellSelection = {
    val initAmmount = initialAmmount(sp)
    val initCurrency = initialCurrency(sp)
    val finAmmount = SellOrder.toAmmount(lastOrder)
    val finCurrency = lastOrder.to
    val profit = finAmmount.fold(BigDecimal(-1))(_ - initAmmount)
    if (finCurrency != initCurrency) noSale("ends in a different currency")
    else if (profit <= 0) noSale(s"no profit ($profit)")
    else SellPath(orders :+ lastOrder)
  }

  @inline def selectBestOutcome(outcomes: NonEmptyList[SellStep], sel: SellSelection): SellSelection =
    outcomes.map(step => step(sel))
      .reduceLeft(bestSell)

}
