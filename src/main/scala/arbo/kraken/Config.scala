package arbo
package kraken

import scodec.bits.ByteVector
import cats.Show

case class Config(apiKey: String, privateKey: ByteVector)

object Config {
  implicit val show = Show.show[Config] { cfg =>
    s"KrakenConfig(${cfg.apiKey.take(6)}...)"
  }
}
