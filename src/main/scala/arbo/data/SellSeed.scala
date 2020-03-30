package arbo
package data

import monocle.Traversal
import monocle.syntax.all._
import monocle.function.all._

object SellSeed {

  def seedPast[O <: SellOrder]: Traversal[List[SellSeed[O]], PastHoldings] =
    each[List[SellSeed[O]], SellSeed[O]].composeLens(_3)

}
