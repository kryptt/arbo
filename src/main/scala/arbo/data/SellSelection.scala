package arbo
package data

import cats.data.NonEmptyList
import cats.Eq

import monocle.Lens
import monocle.function.all._

sealed trait SellSelection[+O <: SellOrder] extends Any with Serializable

object SellSelection {
  case class InitialState(holding: Holding) extends AnyVal with SellSelection[Nothing]

  case class SellPath[+O <: SellOrder](orders: SellSequence[O]) extends AnyVal with SellSelection[O]

  object SellPath {
    def orders[O <: SellOrder]: Lens[SellPath[O], SellSequence[O]] =
      Lens[SellPath[O], SellSequence[O]](_.orders)(os => sp => sp.copy(orders = os))

    def lastOrder[O <: SellOrder]: Lens[SellPath[O], O] =
      orders[O].composeLens(last)
  }

  case class NoSale(reasons: NonEmptyList[String]) extends AnyVal with SellSelection[Nothing]

  def init(holding: Holding): InitialState =
    InitialState(holding)

  def initialCurrency(sp: SellPath[SellOrder]): Currency =
    sp.orders.head.from

  def initialAmmount(sp: SellPath[SellOrder]): Ammount =
    sp.orders.head.fromAmmount

  def finalCurrency(sp: SellPath[SellOrder]): Currency =
    sp.orders.last.to

  def finalAmmount(sp: SellPath[SellOrder]): Option[Ammount] =
    SellOrder.toAmmount(sp.orders.last)

  def finalHolding(sp: SellPath[SellOrder]): Option[Holding] =
    finalAmmount(sp).map(Holding(finalCurrency(sp), _))

  def noSale(reason: String): NoSale =
    NoSale(NonEmptyList.one(reason))

  def firstOrder[O <: SellOrder](order: O): SellPath[O] =
    SellPath(NonEmptyList.one(order))

  def orders[O <: SellOrder](os: NonEmptyList[O]): SellPath[O] =
    SellPath(os)

  def appendToOrders[O <: SellOrder](order: O, orders: NonEmptyList[O]): SellPath[O] =
    SellPath(orders :+ order)

  def bestSell[O <: SellOrder](
      left: SellSelection[O],
      right: SellSelection[O]): SellSelection[O] = {

    @inline def whenSameCurrency(left: SellPath[O], right: SellPath[O]): SellSelection[O] =
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

    @inline def whenSellPath(left: SellPath[O], right: SellPath[O]): SellSelection[O] =
      if (finalCurrency(left) != finalCurrency(right))
        noSale("unexpected finalCurrency mismatch")
      else whenSameCurrency(left, right)

    (left, right) match {
      case (left: SellPath[O], right: SellPath[O]) => whenSellPath(left, right)
      case (left: SellPath[O], _) => left
      case (_, right: SellPath[O]) => right
      case (left, _: InitialState) => left
      case (_: InitialState, right) => right
      case (NoSale(lReasons), NoSale(rReasons)) =>
        NoSale(NonEmptyList(lReasons.head, List(rReasons.head)))
    }
  }

  implicit val eqSellSelection: Eq[SellSelection[SellOrder]] = Eq.instance {
    case (_: NoSale, _: NoSale) => true
    case (InitialState(a), InitialState(b)) => a == b
    case (SellPath(as), SellPath(bs)) => as == bs
    case _ => false
  }
}
