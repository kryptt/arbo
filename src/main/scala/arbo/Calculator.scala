package arbo

import cats.{Applicative, Functor, Monad}
import cats.data.NonEmptyList
import cats.implicits._

import higherkindness.droste
import droste.Algebra

import data._
import elgot._

object Calculator {

  import SellTree._
  import SellSeed._
  import SellSelection._

  def selection[M[_]: Monad](sellOptions: GetSellOptions[M], terminalCurrency: Currency, maxDepth: Depth)(holding: Holding): M[SellSelection] =
    elgotM[M, SellTree, SellSeed, SellStep](
      optionsAlgebra,
      optionsCoalgebra(sellOptions, terminalCurrency, maxDepth))
      .apply(initialSeed(holding))
      .map(_.apply(init(holding)))

  def optionsCoalgebra[M[_]: Applicative](sellOptions: GetSellOptions[M], terminalCurrency: Currency, maxDepth: Depth): ElgotCoalgebraM[M, Either[SellStep, *], SellTree, SellSeed] = {
    ElgotCoalgebraM[M, Either[SellStep, *], SellTree, SellSeed] {
      case (_, _, _, depth) if depth > maxDepth =>
        val excededSel = noSale(s"maximum search depth ($maxDepth) exceeded")
        Applicative[M].pure(Left(_ => excededSel))
      case (Some(pOrder), holding, _, depth) if holding.currency == terminalCurrency =>
        Applicative[M].pure(Right(TerminalNode(pOrder, depth)))
      case (pOrderOpt, holding, past, depth) =>
        buildChildren(sellOptions, pOrderOpt, holding, past, depth)
    }
  }

  def optionsAlgebra: Algebra[SellTree, SellStep] =
    Algebra[SellTree, SellStep] {
      case SellNode(order, _, outcomes) =>
        sellNodeStep(order, outcomes)
      case TerminalNode(lastOrder, _) =>
        terminalNodeStep(lastOrder)
    }

  @inline def initialSeed(holding: Holding): SellSeed =
    (None, holding, Map.empty, 0)

  @inline def buildChildren[M[_]: Functor](sellOptions: GetSellOptions[M], pOrderOpt: Option[SellOrder], holding: Holding, past: PastHoldings, depth: Depth): M[Either[SellStep, SellTree[SellSeed]]] = {
    val pOrder = ensurePreviousOrder(pOrderOpt, holding)
    sellOptions(holding)
      .map(_.flatMap(nextSeed(past, depth)) match {
             case Nil =>
               val exhaustedOptions = noSale(s"options exhausted after digging $depth levels through $past")
               Left(_ => exhaustedOptions)
             case next =>
               val (chld, holds) = next.unzip
               val firstChild :: otherChildren = seedPast.modify(_ ++ holds)(chld)
               Right(SellNode(pOrder, depth, NonEmptyList(firstChild, otherChildren)))
           })
  }

  @inline def nextSeed(past: PastHoldings, depth: Depth): SellOrder => Option[(SellSeed, HoldingTuple)] =
    (order: SellOrder) =>
  SellOrder.nextHolding(order).flatMap {
    case next@Holding(to, ammount) =>
      if (past.get(to).fold(true)(_ < ammount))
        Some((Some(order), next, past, depth + 1) -> (to -> ammount))
      else None
  }


  @inline def ensurePreviousOrder(pOrderOpt: Option[SellOrder], holding: Holding): SellOrder =
    pOrderOpt.getOrElse(SellOrder.emptyOrder(holding))

  @inline def terminalNodeStep(lastOrder: SellOrder): SellSelection => SellSelection = {
    case sp@SellPath(orders) => lastSelection(lastOrder, sp, orders)
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
    val finAmmount = SellOrder.toAmmount(lastOrder)
    val profit = finAmmount.fold(BigDecimal(-1))(_ - initAmmount)
    if (profit <= 0) noSale(s"no profit ($profit)")
    else SellPath(orders :+ lastOrder)
  }

  @inline def selectBestOutcome(outcomes: NonEmptyList[SellStep], sel: SellSelection): SellSelection =
    outcomes.reduceLeftTo(step => step(sel)) {
      (s, step) => bestSell(s, step(sel))
    }

}
