package arbo

import cats.data.NonEmptyList

package object data {

  type Currency = String

  type Ammount = BigDecimal

  type Holdings = List[Holding]

  type HoldingSequence = NonEmptyList[Holding]

  type Price = Ammount

  type SellOptions = List[SellOrder]

  type SellSequence = NonEmptyList[SellOrder]

  type GetSellOptions = Holding => List[SellOrder]

  type SellStep = SellSelection => SellSelection

  type Depth = Int

  type SellSeed = (Option[SellOrder], Holding, Depth)

}
