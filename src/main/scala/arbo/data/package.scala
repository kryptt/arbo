package arbo

import cats.data.NonEmptyList

package object data {

  type Currency = String

  type Ammount = BigDecimal

  type HoldingTuple = (Currency, Ammount)

  type Holdings = List[Holding]

  type HoldingSequence = NonEmptyList[Holding]

  type Price = Ammount

  type SellOptions = List[SellOrder]

  type SellSequence = NonEmptyList[SellOrder]

  type GetSellOptions[F[_]] = Holding => F[SellOptions]

  type SellStep = SellSelection => SellSelection

  type Depth = Int

  type PastHoldings = Map[Currency, Ammount]

  type SellSeed = (Option[SellOrder], Holding, PastHoldings, Depth)

}
