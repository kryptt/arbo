package arbo.data

import cats.data.NonEmptyList

sealed trait SellSelection extends Any with Serializable

object SellSelection {
  case class InitialState(holding: Holding) extends AnyVal with SellSelection
  case class SellPath(orders: SellSequence) extends AnyVal with SellSelection
  case class NoSale(reasons: NonEmptyList[String]) extends AnyVal with SellSelection

  def init(holding: Holding): InitialState =
    InitialState(holding)

  def initialCurrency(sp: SellPath): Currency =
    sp.orders.head.from

  def initialAmmount(sp: SellPath): Ammount =
    sp.orders.head.fromAmmount

  def finalCurrency(sp: SellPath): Currency =
    sp.orders.last.to

  def finalAmmount(sp: SellPath): Option[Ammount] =
    SellOrder.toAmmount(sp.orders.last)

  def finalHolding(sp: SellPath): Option[Holding] =
    finalAmmount(sp).map(Holding(finalCurrency(sp), _))

  def noSale(reason: String): NoSale =
    NoSale(NonEmptyList.one(reason))

  def firstOrder(order: SellOrder): SellPath =
    SellPath(NonEmptyList.one(order))

  def orders(os: NonEmptyList[SellOrder]): SellPath =
    SellPath(os)

  def appendToOrders(order: SellOrder, orders: NonEmptyList[SellOrder]): SellPath =
    SellPath(orders :+ order)

  def bestSell(left: SellSelection, right: SellSelection): SellSelection = {

    @inline def whenSameCurrency(left: SellPath, right: SellPath): SellSelection =
      (finalAmmount(left), finalAmmount(right)) match {
        case (None, None) => noSale("incomplete sellPath")
        case (Some(_), None) => left
        case (None, Some(_)) => right
        case (Some(lA), Some(rA)) =>
          val diff = rA - lA
          if (diff > 0) right
          else if (diff < 0) left
          else if (left.orders.length > right.orders.length) right
          else left
      }

    @inline def whenSellPath(left: SellPath, right: SellPath): SellSelection =
      if (finalCurrency(left) != finalCurrency(right))
        noSale("unexpected finalCurrency mismatch")
      else whenSameCurrency(left, right)

    (left, right) match {
      case (left: SellPath, right: SellPath) => whenSellPath(left, right)
      case (left: SellPath, _) => left
      case (_, right: SellPath) => right
      case (left, _: InitialState) => left
      case (_: InitialState, right) => right
      case (NoSale(lReasons), NoSale(rReasons)) =>
        NoSale(NonEmptyList(lReasons.head, List(rReasons.head)))
    }
  }
}
