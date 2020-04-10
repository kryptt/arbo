package arbo
package kraken

import org.apache.commons.codec.binary.Base64
import scodec.bits.ByteVector

import io.circe.Json
import io.circe.syntax._

import org.http4s.{HttpApp, Request, Response}
import org.http4s.client.Client
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.syntax.string._

import cats.effect.IO
import cats.effect.concurrent.Deferred
import cats.syntax.functor._

import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.{IOMatchers, OptionMatchers}
import org.scalacheck.Prop

class RestClientSpec
    extends Specification
    with IOMatchers
    with OptionMatchers
    with ScalaCheck
    with Http4sDsl[IO] {
  def is = s2"""
    Correctly signs the execute request $signedReq
"""

  import scala.concurrent.ExecutionContext.global
  implicit val CS = IO.contextShift(global)
  import Generators._

  val cfg = Config(
    apiKey = "7jHdFe9VrH4bSsQrAnKjrRBGMeAZym4a2xQcIe2+tjF9mt4ZTzpcRUx7",
    privateKey = ByteVector(
      Base64.decodeBase64(
        "y59eY91LNC73oXk6bIz86eCmAvh8WMglFvCY9BVh8WMglFvCY9BVU5TRDWSrPcZbmzAQTJrDDbx6UGkjSwcc2EzIzRWeH7Z2psA=="
          .getBytes()))
  )

  def recordingCli: IO[(Deferred[IO, Request[IO]], Client[IO])] =
    Deferred[IO, Request[IO]].fproduct { defferedRequest =>
      Client.fromHttpApp[IO](HttpApp[IO] { req: Request[IO] =>
        val json = Json.obj("accepted" := "yes")
        defferedRequest
          .complete(req)
          .as(Response[IO](Ok).withEntity(json))
      })
    }

  def signedReq =
    Prop
      .forAll(orderGen) {
        case (order, _, _) =>
        val sentKey: IO[Option[String]] = for {
          rc <- recordingCli
          (defferedRequest, cli) = rc
          _ <- RestClient[IO](cfg, cli).use(_.execute(order)).attempt
          req <- defferedRequest.get
        } yield req.headers.get("API-Key".ci).map(_.value)

        def confirmKey(os: Option[String]) =
          os.must(beSome("7jHdFe9VrH4bSsQrAnKjrRBGMeAZym4a2xQcIe2+tjF9mt4ZTzpcRUx7"))

        sentKey.must(returnValue(confirmKey(_)))
      }

}
