package arbo.kraken

import cats.effect.Sync

import org.http4s.Method._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._

trait RestClient[F[_]] {
  def assetPairs: F[FeesResponse]
}

object RestClient {

  val baseURI = uri"https://api.kraken.com/0/public/"

  def apply[F[_]: Sync](C: Client[F]) = new RestClient[F] {
    val dsl = new Http4sClientDsl[F]{}
    import dsl._
    import FeesResponse._

    def assetPairs: F[FeesResponse] = {
      val req = GET(baseURI / "AssetPairs" +?("info", "fees"))
      C.expect[FeesResponse](req)
    }
  }

}
