package arbo
package data

import org.scalacheck.{Arbitrary, Cogen}
import org.specs2.Specification
import cats.kernel.laws.discipline._
import org.typelevel.discipline.specs2.Discipline
import org.typelevel.discipline.Laws
import cats.{Id, Semigroup}
import cats.instances.option._
import cats.instances.int._
import cats.instances.long._
import cats.instances.double._
import arbo.data.SellSelection.SellPath
import cats.kernel.Eq
import cats.laws.discipline.TraverseTests

class DataSpec extends Specification with Discipline with Laws {
  def is = s2"""
    $holdingIsPartialOrder
    (- associativity) $sellSelectionIsSemigroup
    $sellPathSameFinalCurrencySemigroup
    $sellTreeTraverse
"""

  implicit val holdingFCoGen: Cogen[Holding] = Cogen[BigDecimal].contramap(_.ammount)

  implicit val sellSelectionSemigroup: Semigroup[SellSelection[SellOrder]] =
    Semigroup.instance(SellSelection.bestSell[SellOrder])

  implicit val arbHolding = Arbitrary(Generators.holdingGen)
  implicit val arbSellSelection = Arbitrary(Generators.sellSelectionGen)
  implicit val sellOrder = Arbitrary(Generators.sellOrderGen)

  implicit def arbSellTree[A: Arbitrary] = Arbitrary(Generators.sellTreeGen[A])

  val eurSellPathGen = Generators.sellPathGen
    .map(SellPath.lastOrder.modify((o: SellOrder) => o.copy(to = "EUR", fee = Fee(0, "EUR"))))

  def holdingIsPartialOrder =
    checkAll("Holding", PartialOrderTests[Holding].partialOrder)

  def sellSelectionIsSemigroup =
    checkAll("SellSelection", {
      val rs = SemigroupTests[SellSelection[SellOrder]].semigroup
      new DefaultRuleSet(rs.name, None, rs.props.filterNot(_._1 == "associative"): _*)
    })

  def sellPathSameFinalCurrencySemigroup = {
    val semigroup = sellSelectionSemigroup.asInstanceOf[Semigroup[SellPath[SellOrder]]]
    val eqInstance = SellSelection.eqSellSelection.asInstanceOf[Eq[SellPath[SellOrder]]]
    checkAll(
      "SameCurrencySellPath",
      SemigroupTests[SellPath[SellOrder]](semigroup)
        .semigroup(Arbitrary(eurSellPathGen), eqInstance))
  }

  def sellTreeTraverse =
    checkAll(
      "SellTree",
      TraverseTests[SellTree[*, SellOrder]]
        .traverse[Int, Long, Double, Int, Id, Option])

}
