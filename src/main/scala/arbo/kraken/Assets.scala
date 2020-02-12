package arbo
package kraken

import org.http4s.circe._
import org.http4s.EntityDecoder

import io.circe._

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.instances.either._
import cats.instances.option._
import cats.syntax.apply._

import scala.util.{Try, Success, Failure}

import data._

case class FeeOption (
  volume: Int,
  ammount: Ammount,
)

object FeeOption {

  object FeeOptionArrayMismatchException extends RuntimeException("FeeOption Array Mismatch Exception")

  implicit val feeOptionDecoder: Decoder[FeeOption] = Decoder.instanceTry[FeeOption] { (c) =>
    @inline def err: Try[FeeOption] = Failure(FeeOptionArrayMismatchException)
    c.as[List[JsonNumber]].toTry.flatMap {
      case List(v, a) =>
        (v.toInt, a.toBigDecimal)
          .mapN(FeeOption.apply _)
          .fold(err)(Success(_))
      case _ => err
    }
  }

}

case class FeeOptions (
  currency: Currency,
  taker: NonEmptyList[FeeOption],
  maker: List[FeeOption],
  )

object FeeOptions {

  implicit val feeOptionsDecoder: Decoder[FeeOptions] = new Decoder[FeeOptions] {
    final def apply(c: HCursor): Decoder.Result[FeeOptions] =
      (c.downField("fee_volume_currency").as[Currency],
       c.downField("fees").as[NonEmptyList[FeeOption]],
       c.downField("fees_maker")
         .as[Option[List[FeeOption]]]
         .map(_.getOrElse(Nil))
      ).mapN(FeeOptions.apply _)
  }

}

case class CurrencyPair (
  from: Currency,
  to: Currency
)

object CurrencyPair {

  val cPattern = "([XZ]?\\w{3}(?:\\.d)?|DASH|ALGO|ATOM|USD[CT]|LINK|WAVES|NANO|PAXG|QTUM|SC)"
  val cRE = (cPattern * 2).r

  implicit val currencyPairKeyDecoder =
    KeyDecoder.instance(fromString _)

  def fromString(str: String): Option[CurrencyPair] = str match {
    case cRE(from, to) => Some(CurrencyPair(from, to))
    case _ => None
  }
}

object FeesResponse {
  implicit val feesResponseDecoder: Decoder[FeesResponse] = new Decoder[FeesResponse] {
    final def apply(c: HCursor): Decoder.Result[FeesResponse] =
      c.downField("result").as[FeesResponse](Decoder.decodeMap[CurrencyPair, FeeOptions])
  }

  implicit def feesResponseEntityDecoder[F[_]: Sync]: EntityDecoder[F, FeesResponse] =
    jsonOf

}
