package arbo
package data

import monocle.Traversal
import monocle.syntax.all._
import monocle.function.all._

object SellSeed {

  val seedPast: Traversal[List[SellSeed], PastHoldings] =
    each[List[SellSeed], SellSeed] composeLens _3

}
