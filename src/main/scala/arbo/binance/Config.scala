package arbo
package binance

import scodec.bits.ByteVector
import cats.Show

/**
 * Binance configuration with API key/secret and Show instance for redaction.
 * 
 * This configuration class holds the necessary credentials and settings
 * for connecting to the Binance API, with proper credential redaction
 * for security.
 */
case class Config(
  apiKey: String,
  secretKey: ByteVector,
  testnet: Boolean = false
)

object Config {
  implicit val show = Show.show[Config] { cfg =>
    s"BinanceConfig(${cfg.apiKey.take(6)}..., testnet=${cfg.testnet})"
  }
}
