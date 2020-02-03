package arbo.data

import cats.data.{Ior, NonEmptyList}

sealed trait SellSelection

object SellSelection {
  case class InitialState(holding: Holding) extends SellSelection
  case class SellPath(orders: SellSequence) extends SellSelection

  def init(holding: Holding) = InitialState(holding)

  def initialCurrency(sp: SellPath): Currency = sp.orders.head.from
  def initialAmmount(sp: SellPath): Ammount = sp.orders.head.fromAmmount
  def finalCurrency(sp: SellPath): Currency = sp.orders.last.to
  def finalAmmount(sp: SellPath): Option[Ammount] = SellOrder.toAmmount(sp.orders.last)

  def finalHolding(sp: SellPath): Option[Holding] =
    finalAmmount(sp).map(Holding(finalCurrency(sp), _))

  case class NoSale(reasons: NonEmptyList[String]) extends SellSelection

  def noSale(reason: String) = NoSale(NonEmptyList.one(reason))

  def firstOrder(order: SellOrder) = SellPath(NonEmptyList.one(order))

  def appendOrder(order: SellOrder, orders: NonEmptyList[SellOrder]) = SellPath(orders :+ order)

  def orders(os: NonEmptyList[SellOrder]) = SellPath(os)

  def bestSell(requiredCurrency: Currency)(left: SellSelection, right: SellSelection): SellSelection = {
    println("bestSell")
    def whenSameCurrency(left: SellPath, right: SellPath) = {
      Ior
        .fromOptions(finalAmmount(left), finalAmmount(right))
        .fold[SellSelection](noSale("incomplete sellPath"))(_.fold(_ => left,
              _ => right,
              (lA, rA) => {
                val diff = rA - lA
                if (diff > 0) right
                else if (diff < 0) left
                else if (left.orders.length > right.orders.length) right
                else left
              }))
    }
    def whenSellPath(left: SellPath, right: SellPath) =
      (finalCurrency(left), finalCurrency(right)) match {
      case (lC, rC) if lC == rC && lC == requiredCurrency => whenSameCurrency(left, right)
      case (lC, _) if lC == requiredCurrency => left
      case (_, rC) if rC == requiredCurrency => right
      case _ => left
      }
        (left, right) match {
        case (left: SellPath, right: SellPath) => whenSellPath(left, right)
        case (left: SellPath, _) => left
        case (_, right: SellPath) => right
        case (left, _: InitialState) => left
        case (_: InitialState, right) => right
        case (NoSale(lReasons), NoSale(rReasons)) => NoSale(lReasons ::: rReasons)
      }
  }
}
