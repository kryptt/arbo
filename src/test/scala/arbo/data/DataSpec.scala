package arbo
package data

import org.scalacheck.{Arbitrary, Cogen}
import org.specs2.Specification
import cats.kernel.laws.discipline._
import org.typelevel.discipline.specs2.Discipline
import org.typelevel.discipline.Laws
import cats.Semigroup
import cats.instances.option._

class DataSpec extends Specification with Discipline with Laws {
  def is = s2"""
    $holdingIsPartialOrder
    (- associativity) $sellSelectionIsSemigroup
"""

  implicit val holdingFCoGen: Cogen[Holding] = Cogen[BigDecimal].contramap(_.ammount)

  implicit val sellSelectionSemigroup: Semigroup[SellSelection[SellOrder]] =
    Semigroup.instance(SellSelection.bestSell[SellOrder])

  implicit val arbHolding = Arbitrary(Generators.holdingGen)
  implicit val arbSellSelection = Arbitrary(Generators.sellSelectionGen)

  def holdingIsPartialOrder =
    checkAll("Holding", PartialOrderTests[Holding].partialOrder)

  def sellSelectionIsSemigroup =
    checkAll("SellSelection", {
      val rs = SemigroupTests[SellSelection[SellOrder]].semigroup
      new DefaultRuleSet(rs.name, None, rs.props.filterNot(_._1 == "associative"): _*)
    })

}
