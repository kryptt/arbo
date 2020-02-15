package arbo.kraken

import cats.data.NonEmptyList
import cats.effect.Sync

import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._

trait RestClient[F[_]] {
  def assetPairs: F[FeesResponse]
  def ticker(pair: NonEmptyList[CurrencyPair]): F[TickerResponse]
}

object RestClient {

  val baseURI = uri"https://api.kraken.com/0/public/"

  def apply[F[_]: Sync](C: Client[F]) = new RestClient[F] {
    val dsl = new Http4sClientDsl[F]{}
    import dsl._

    def assetPairs: F[FeesResponse] = {
      import FeesResponse._
      val req = GET(baseURI / "AssetPairs" +?("info", "fees"))
      C.expect[FeesResponse](req)
    }

    def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse] = {
      import TickerResponse._
      val pairparam = pairs.toList.map(CurrencyPair.toKraken).mkString(",")
      val req = GET(baseURI / "Ticker" +?("pair", pairparam))
      C.expect[TickerResponse](req)
    }
  }

}
