package arbo
package data

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.cats.implicits._

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.apply._

object Generators {

  import SellSelection._
  import SellTree._

  val currencyList = List("EUR", "XBT", "ETH", "USD", "ADA", "XMR", "XRP", "TZK", "USDT", "ETC")

  val currencyGen: Gen[Currency] =
    Gen.oneOf(currencyList)

  val ammountGen: Gen[Ammount] =
    Gen.posNum[Int].map(i => BigDecimal(i) / 100000000)

  val holdingGen: Gen[Holding] =
    Applicative[Gen].map2(currencyGen, ammountGen)(Holding.apply _)

  val priceGen: Gen[Price] =
    Gen.chooseNum(0.000005, 20000).map(d => BigDecimal(d))

  def genHoldingSellOrder(holding: Holding): Gen[SellOrder] =
    for {
      to <- Gen.oneOf(currencyList.filter(_ != holding.currency))
      price <- priceGen
      fee <- Gen.oneOf(holding.currency, to)
      feeAmmount <- ammountGen
    } yield SellOrder(
      holding.currency,
      to,
      price,
      holding.ammount,
      Fee(feeAmmount, fee)
    )

  val sellOrderGen: Gen[SellOrder] =
    holdingGen.flatMap(genHoldingSellOrder)

  def getSellOptionsGen(holding: Holding): Gen[SellOptions[SellOrder]] =
    Gen.listOf(genHoldingSellOrder(holding))

  val sellPathGen: Gen[SellPath[SellOrder]] = Gen
    .nonEmptyListOf(sellOrderGen)
    .map(NonEmptyList.fromListUnsafe)
    .map(orders)

  val sellSelectionGen: Gen[SellSelection[SellOrder]] =
    Gen.oneOf("init", "path", "nosale").flatMap {
      case "init" => holdingGen.map(InitialState)
      case "nosale" => Gen.alphaStr.map(noSale)
      case "path" => sellPathGen
    }

  val depthGen = Gen.chooseNum[Int](2, 12)

  def sellTreeGen[A: Arbitrary]: Gen[SellTree[A, SellOrder]] = {
    val childGen =
      Gen
        .nonEmptyListOf(Arbitrary.arbitrary[A])
        .map(NonEmptyList.fromListUnsafe)
    Gen.oneOf("root", "node", "terminal").flatMap {
      case "root" =>
        childGen
          .map(RootNode[A])
      case "node" =>
        (sellOrderGen, depthGen, childGen)
          .mapN(SellNode[A, SellOrder])
      case "terminal" =>
        (sellOrderGen, depthGen)
          .mapN(TerminalNode[SellOrder])
    }
  }

}
