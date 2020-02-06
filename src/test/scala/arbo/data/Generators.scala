package arbo.data

import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import cats.Applicative

object Generators {

  val currencyList = List("EUR", "XBT", "ETH", "USD", "ADA", "XMR", "XRP", "TZK", "USDT", "ETC")

  val currencyGen: Gen[Currency] =
    Gen.oneOf(currencyList)

  val ammountGen: Gen[Ammount] =
    Gen.posNum[Int].map(i => BigDecimal(i) / 100000000)

  val holdingGen: Gen[Holding] =
    Applicative[Gen].map2(currencyGen, ammountGen)(Holding.apply _)

  val priceGen: Gen[Price] =
    Gen.chooseNum(0.000005, 20000).map(d => BigDecimal(d))

  def genHoldingSellOrder(holding: Holding): Gen[SellOrder] = for {
    to <- Gen.oneOf(currencyList.filter(_ != holding.currency))
    price <- priceGen
    fee <- Gen.oneOf(holding.currency, to)
    feeAmmount <- ammountGen
  } yield SellOrder (
    holding.currency,
    to,
    price,
    holding.ammount,
    Fee(feeAmmount, fee)
  )

  val sellOrderGen: Gen[SellOrder] =
    holdingGen.flatMap(genHoldingSellOrder)

  def getSellOptionsGen(holding: Holding): Gen[SellOptions] =
    Gen.listOf(genHoldingSellOrder(holding))

}
