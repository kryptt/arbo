package arbo

import cats.data.NonEmptyList

package object data {

  type Currency = String

  type Ammount = BigDecimal

  type HoldingTuple = (Currency, Ammount)

  type Holdings = List[Holding]

  type HoldingSequence = NonEmptyList[Holding]

  type Price = Ammount

  type SellOptions[+O <: SellOrder] = List[O]

  type SellSequence[+O <: SellOrder] = NonEmptyList[O]

  type GetSellOptions[F[_], O <: SellOrder] = Holding => F[SellOptions[O]]

  type SellStep[O <: SellOrder] = SellSelection[O] => SellSelection[O]

  type Depth = Int

  type PastHoldings = Map[Currency, Ammount]

  type SellSeed[+O <: SellOrder] = (Option[O], Holding, PastHoldings, Depth)

}
