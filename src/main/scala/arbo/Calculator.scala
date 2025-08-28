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

  def selection[M[_]: Monad, O <: SellOrder](
      sellOptions: GetSellOptions[M, O],
      terminalCurrency: Currency,
      maxDepth: Depth)(holding: Holding): M[SellSelection[O]] =
    elgotM[M, SellTree[*, O], SellSeed[O], SellStep[O]](
      optionsAlgebra,
      optionsCoalgebra(sellOptions, terminalCurrency, maxDepth))
      .apply(initialSeed(holding))
      .map(_.apply(init(holding)))

  def optionsCoalgebra[M[_]: Applicative, O <: SellOrder](
      sellOptions: GetSellOptions[M, O],
      terminalCurrency: Currency,
      maxDepth: Depth): ElgotCoalgebraM[M, Either[SellStep[O], *], SellTree[*, O], SellSeed[O]] =
    ElgotCoalgebraM[M, Either[SellStep[O], *], SellTree[*, O], SellSeed[O]] {
      case (_, _, _, depth) if depth > maxDepth =>
        val excededSel = noSale(s"maximum search depth ($maxDepth) exceeded")
        Applicative[M].pure(Left(_ => excededSel))
      case (Some(pOrder), holding, _, depth) if holding.currency == terminalCurrency =>
        Applicative[M].pure(Right(TerminalNode(pOrder, depth)))
      case (pOrderOpt, holding, past, depth) =>
        buildChildren(sellOptions, pOrderOpt, holding, past, depth)
    }

  def optionsAlgebra[O <: SellOrder]: Algebra[SellTree[*, O], SellStep[O]] =
    Algebra[SellTree[*, O], SellStep[O]] {
      case RootNode(outcomes) =>
        rootNodeStep(outcomes)
      case SellNode(order, _, outcomes) =>
        sellNodeStep(order, outcomes)
      case TerminalNode(lastOrder, _) =>
        terminalNodeStep(lastOrder)
    }

  @inline def initialSeed(holding: Holding): SellSeed[Nothing] =
    (None, holding, Map.empty, 0)

  @inline def buildChildren[M[_]: Functor, O <: SellOrder](
      sellOptions: GetSellOptions[M, O],
      pOrderOpt: Option[O],
      holding: Holding,
      past: PastHoldings,
      depth: Depth): M[Either[SellStep[O], SellTree[SellSeed[O], O]]] =
    sellOptions(holding)
      .map(_.flatMap(nextSeed(past, depth)) match {
        case Nil =>
          val exhaustedOptions =
            noSale(s"options exhausted after digging $depth levels through $past")
          Left(_ => exhaustedOptions)
        case next =>
          val (chld, holds) = next.unzip
          val firstChild :: otherChildren = seedPast.modify(_ ++ holds)(chld)
          Right(ensureTreeNode(pOrderOpt, depth, NonEmptyList(firstChild, otherChildren)))
      })

  @inline def nextSeed[O <: SellOrder](
      past: PastHoldings,
      depth: Depth): O => Option[(SellSeed[O], HoldingTuple)] =
    (order: O) =>
      SellOrder.nextHolding(order).flatMap {
        case next @ Holding(to, ammount) =>
          if (past.get(to).fold(true)(_ < ammount))
            Some((Some(order), next, past, depth + 1) -> (to -> ammount))
          else None
      }

  @inline def ensureTreeNode[A, O <: SellOrder](
      po: Option[O],
      depth: Int,
      ch: NonEmptyList[A]): SellTree[A, O] =
    po.fold[SellTree[A, O]](RootNode(ch))(o => SellNode(o, depth, ch))

  @inline def terminalNodeStep[O <: SellOrder](lastOrder: O): SellSelection[O] => SellSelection[O] = {
    case sp @ SellPath(orders) => lastSelection(lastOrder, sp, orders)
    case _: InitialState => noSale("no sales selected")
    case ns: NoSale => ns
  }

  @inline def rootNodeStep[O <: SellOrder](
      outcomes: NonEmptyList[SellStep[O]]): SellSelection[O] => SellSelection[O] = {
    case is: InitialState =>
      selectBestOutcome(outcomes, is)
    case sp: SellPath[O] =>
      selectBestOutcome(outcomes, sp)
    case ns: NoSale => ns
  }

  @inline def sellNodeStep[O <: SellOrder](
      order: O,
      outcomes: NonEmptyList[SellStep[O]]): SellSelection[O] => SellSelection[O] = {
    case SellPath(orders) =>
      selectBestOutcome(outcomes, appendToOrders(order, orders))
    case _: InitialState =>
      selectBestOutcome(outcomes, firstOrder(order))
    case ns: NoSale => ns
  }

  @inline def lastSelection[O <: SellOrder](
      lastOrder: O,
      sp: SellPath[O],
      orders: SellSequence[O]): SellSelection[O] = {
    val initAmmount = initialAmmount(sp)
    val finAmmount = SellOrder.toAmmount(lastOrder)
    val profit = finAmmount.fold(BigDecimal(-1))(_ - initAmmount)
    if (profit <= 0) noSale(s"no profit ($profit)")
    else SellPath(orders :+ lastOrder)
  }

  @inline def selectBestOutcome[O <: SellOrder](
      outcomes: NonEmptyList[SellStep[O]],
      sel: SellSelection[O]): SellSelection[O] =
    outcomes.reduceLeftTo(step => step(sel)) { (s, step) =>
      bestSell(s, step(sel))
    }

}

  // Multi-exchange support

  import exchanges.Exchange
  import MultiExchangePath._

  /**
   * Multi-exchange selection method accepting NonEmptyList[Exchange[M]] for cross-exchange analysis.
   */
  def multiExchangeSelection[M[_]: Monad, O <: SellOrder](
    exchanges: NonEmptyList[Exchange[M, O]],
    terminalCurrency: Currency,
    maxDepth: Depth
  )(holding: Holding): M[MultiExchangePath[O]] = {
    elgotM[M, SellTree[*, ExchangeOrder[O]], SellSeed[ExchangeOrder[O]], SellStep[ExchangeOrder[O]]](
      multiExchangeAlgebra,
      multiExchangeCoalgebra(exchanges, terminalCurrency, maxDepth))
      .apply(initialMultiExchangeSeed(holding))
      .map(_.apply(init(holding)))
  }

  /**
   * Multi-exchange coalgebra that considers cross-exchange paths in tree generation.
   */
  def multiExchangeCoalgebra[M[_]: Applicative, O <: SellOrder](
    exchanges: NonEmptyList[Exchange[M, O]],
    terminalCurrency: Currency,
    maxDepth: Depth
  ): ElgotCoalgebraM[M, Either[SellStep[ExchangeOrder[O]], *], SellTree[*, ExchangeOrder[O]], SellSeed[ExchangeOrder[O]]] =
    ElgotCoalgebraM[M, Either[SellStep[ExchangeOrder[O]], *], SellTree[*, ExchangeOrder[O]], SellSeed[ExchangeOrder[O]]] {
      case (_, _, _, depth) if depth > maxDepth =>
        val exceededSel = noSale(s"maximum search depth ($maxDepth) exceeded")
        Applicative[M].pure(Left(_ => exceededSel))
      case (Some(pOrder), holding, _, depth) if holding.currency == terminalCurrency =>
        Applicative[M].pure(Right(TerminalNode(pOrder, depth)))
      case (pOrderOpt, holding, past, depth) =>
        buildMultiExchangeChildren(exchanges, pOrderOpt, holding, past, depth)
    }

  /**
   * Multi-exchange algebra for processing tree nodes with exchange routing information.
   */
  def multiExchangeAlgebra[O <: SellOrder]: Algebra[SellTree[*, ExchangeOrder[O]], SellStep[ExchangeOrder[O]]] =
    Algebra[SellTree[*, ExchangeOrder[O]], SellStep[ExchangeOrder[O]]] {
      case RootNode(outcomes) =>
        multiExchangeRootNodeStep(outcomes)
      case SellNode(order, _, outcomes) =>
        multiExchangeSellNodeStep(order, outcomes)
      case TerminalNode(lastOrder, _) =>
        multiExchangeTerminalNodeStep(lastOrder)
    }

  @inline def initialMultiExchangeSeed(holding: Holding): SellSeed[ExchangeOrder[Nothing]] =
    (None, holding, Map.empty, 0)

  @inline def buildMultiExchangeChildren[M[_]: Functor, O <: SellOrder](
    exchanges: NonEmptyList[Exchange[M, O]],
    pOrderOpt: Option[ExchangeOrder[O]],
    holding: Holding,
    past: PastHoldings,
    depth: Depth
  ): M[Either[SellStep[ExchangeOrder[O]], SellTree[SellSeed[ExchangeOrder[O]], ExchangeOrder[O]]]] = {
    // Get sell options from all exchanges
    val allOptions = exchanges.traverse { exchange =>
      exchange.getSellOptions(holding).map { options =>
        options.map(order => ExchangeOrder(order, exchange.name))
      }
    }
    
    allOptions.map { exchangeOptions =>
      val allSellOptions = exchangeOptions.flatten
      allSellOptions.flatMap(nextMultiExchangeSeed(past, depth)) match {
        case Nil =>
          val exhaustedOptions = noSale(s"options exhausted after digging $depth levels through $past")
          Left(_ => exhaustedOptions)
        case next =>
          val (chld, holds) = next.unzip
          val firstChild :: otherChildren = seedPast.modify(_ ++ holds)(chld)
          Right(ensureTreeNode(pOrderOpt, depth, NonEmptyList(firstChild, otherChildren)))
      }
    }
  }

  @inline def nextMultiExchangeSeed[O <: SellOrder](
    past: PastHoldings,
    depth: Depth
  ): ExchangeOrder[O] => Option[(SellSeed[ExchangeOrder[O]], HoldingTuple)] =
    (exchangeOrder: ExchangeOrder[O]) =>
      SellOrder.nextHolding(exchangeOrder).flatMap {
        case next @ Holding(to, ammount) =>
          if (past.get(to).fold(true)(_ < ammount))
            Some((Some(exchangeOrder), next, past, depth + 1) -> (to -> ammount))
          else None
      }

  @inline def multiExchangeTerminalNodeStep[O <: SellOrder](
    lastOrder: ExchangeOrder[O]
  ): MultiExchangePath[O] => MultiExchangePath[O] = {
    case ep @ ExchangePath(orders, _, _, _) => 
      lastMultiExchangeSelection(lastOrder, ep, orders)
    case _: InitialState => noSale("no sales selected")
    case ns: NoSale => ns
  }

  @inline def multiExchangeRootNodeStep[O <: SellOrder](
    outcomes: NonEmptyList[SellStep[ExchangeOrder[O]]]
  ): MultiExchangePath[O] => MultiExchangePath[O] = {
    case is: InitialState =>
      selectBestMultiExchangeOutcome(outcomes, is)
    case ep: ExchangePath[O] =>
      selectBestMultiExchangeOutcome(outcomes, ep)
    case ns: NoSale => ns
  }

  @inline def multiExchangeSellNodeStep[O <: SellOrder](
    order: ExchangeOrder[O],
    outcomes: NonEmptyList[SellStep[ExchangeOrder[O]]]
  ): MultiExchangePath[O] => MultiExchangePath[O] = {
    case ExchangePath(orders, _, _, _) =>
      selectBestMultiExchangeOutcome(outcomes, appendToOrders(order, orders))
    case _: InitialState =>
      selectBestMultiExchangeOutcome(outcomes, firstOrder(order))
    case ns: NoSale => ns
  }

  @inline def lastMultiExchangeSelection[O <: SellOrder](
    lastOrder: ExchangeOrder[O],
    ep: ExchangePath[O],
    orders: SellSequence[ExchangeOrder[O]]
  ): MultiExchangePath[O] = {
    val initAmmount = initialAmmount(ep)
    val finAmmount = SellOrder.toAmmount(lastOrder)
    val profit = finAmmount.fold(BigDecimal(-1))(_ - initAmmount)
    if (profit <= 0) noSale(s"no profit ($profit)")
    else {
      val newOrders = orders :+ lastOrder
      val totalFees = newOrders.toList.map(_.fee.ammount).sum
      val exchangesUsed = newOrders.toList.map(_.exchangeName).toSet
      
      ExchangePath(
        orders = newOrders,
        totalProfit = profit,
        totalFees = totalFees,
        exchangesUsed = exchangesUsed
      )
    }
  }

  @inline def selectBestMultiExchangeOutcome[O <: SellOrder](
    outcomes: NonEmptyList[SellStep[ExchangeOrder[O]]],
    sel: MultiExchangePath[O]
  ): MultiExchangePath[O] = {
    outcomes.reduceLeftTo(step => step(sel)) { (s, step) =>
      bestMultiExchangePath(s, step(sel))
    }
  }
}
