package arbo
package kraken

import org.http4s.circe._
import org.http4s.EntityDecoder

import io.circe._

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.{Apply, Order}
import cats.instances.either._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._

import scala.util.{Failure, Success, Try}

import data._

case class FeeOption(
    volume: Int,
    percentage: Ammount
)

object FeeOption {

  object FeeOptionArrayMismatchException
      extends RuntimeException("FeeOption Array Mismatch Exception")

  implicit val feeOptionDecoder: Decoder[FeeOption] = Decoder.instanceTry[FeeOption] { (c) =>
    @inline def err: Try[FeeOption] = Failure(FeeOptionArrayMismatchException)
    c.as[List[JsonNumber]].toTry.flatMap {
      case List(v, a) =>
        Apply[Option]
          .map2(v.toInt, a.toBigDecimal)(FeeOption.apply _)
          .fold(err)(Success(_))
      case _ => err
    }
  }

}

case class AssetPairOptions(
  pairDecimals: Int,
  lotDecimals: Int,
  volumeCurrency: Currency,
  taker: NonEmptyList[FeeOption],
  maker: List[FeeOption],
)

object AssetPairOptions {

  implicit val feeOptionsDecoder: Decoder[AssetPairOptions] = new Decoder[AssetPairOptions] {
    final def apply(c: HCursor): Decoder.Result[AssetPairOptions] =
      (
        c.downField("pair_decimals").as[Int],
        c.downField("lot_decimals").as[Int],
        c.downField("fee_volume_currency").as[Currency],
        c.downField("fees").as[NonEmptyList[FeeOption]],
        c.downField("fees_maker")
          .as[Option[List[FeeOption]]]
          .map(_.getOrElse(Nil))).mapN(AssetPairOptions.apply _)
  }

}

case class CurrencyPair(
    base: Currency,
    cvar: Currency,
    )

object CurrencyPair {

  val cPattern = "([XZ]?\\w{3}(?:\\.d)?|DASH|ALGO|ATOM|USD[CT]|LINK|WAVES|NANO|PAXG|QTUM|SC)"
  val cRE = (cPattern * 2).r

  implicit val cpOrder: Order[CurrencyPair] = Order.by(_.base)

  implicit val currencyPairKeyDecoder =
    KeyDecoder.instance(fromString _)

  def fromString(str: String): Option[CurrencyPair] = str match {
    case cRE(base, cvar) => Some(CurrencyPair(base, cvar))
    case _ => None
  }

  def holds(cp: CurrencyPair, h: Holding): Boolean =
    cp.base == h.currency || cp.cvar  == h.currency


  def toKraken(cp: CurrencyPair): String = cp.base + cp.cvar

}

object AssetPairsInfo {
  implicit val feesResponseDecoder: Decoder[AssetPairsInfo] = new Decoder[AssetPairsInfo] {
    final def apply(c: HCursor): Decoder.Result[AssetPairsInfo] =
      c.downField("result").as(Decoder.decodeMap[CurrencyPair, AssetPairOptions])
  }

  implicit def feesResponseEntityDecoder[F[_]: Sync]: EntityDecoder[F, AssetPairsInfo] =
    jsonOf

}

case class Ticker(
    ask: Price,
    bid: Price,
    closed: Price,
    low: Price,
    high: Price,
    open: Price
)

object Ticker {
  implicit val tickerDecoder: Decoder[Ticker] = new Decoder[Ticker] {
    final def apply(c: HCursor): Decoder.Result[Ticker] =
      (
        c.downField("a").as[NonEmptyList[BigDecimal]].map(_.head),
        c.downField("b").as[NonEmptyList[BigDecimal]].map(_.head),
        c.downField("c").as[NonEmptyList[BigDecimal]].map(_.head),
        c.downField("l").as[NonEmptyList[BigDecimal]].map(_.head),
        c.downField("h").as[NonEmptyList[BigDecimal]].map(_.head),
        c.downField("o").as[BigDecimal]).mapN(Ticker.apply _).left.map { e =>
        println(s"failed to parse ticker ${c.value}")
        e
      }
  }
}

object TickerResponse {
  implicit val tickerResponseDecoder: Decoder[TickerResponse] = new Decoder[TickerResponse] {
    final def apply(c: HCursor): Decoder.Result[TickerResponse] =
      c.downField("result").as(Decoder.decodeMap[CurrencyPair, Ticker]).left.map { e =>
        println(s"error parsing ticker result $e \n \n${c.value} \n")
        e
      }
  }

  implicit def tickerResponseEntityDecoder[F[_]: Sync]: EntityDecoder[F, TickerResponse] =
    jsonOf

}
