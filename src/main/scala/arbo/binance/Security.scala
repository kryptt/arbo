package arbo
package binance

import scodec.bits.ByteVector
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/**
 * HMAC SHA256 signature implementation for Binance authentication.
 * 
 * This object provides the cryptographic functions needed to authenticate
 * requests to the Binance API using HMAC SHA256 signatures.
 */
object Security {

  /**
   * Create HMAC SHA256 signature for Binance API requests.
   * 
   * @param queryString The query string to sign
   * @param secretKey The secret key as ByteVector
   * @return The base64-encoded signature
   */
  def sign(queryString: String, secretKey: ByteVector): String = {
    val mac = Mac.getInstance("HmacSHA256")
    val secretKeySpec = new SecretKeySpec(secretKey.toArray, "HmacSHA256")
    mac.init(secretKeySpec)
    
    val signature = mac.doFinal(queryString.getBytes(StandardCharsets.UTF_8))
    ByteVector(signature).toBase64
  }

  /**
   * Create HMAC SHA256 signature for Binance API requests with timestamp and recvWindow.
   * 
   * @param queryString The base query string (without timestamp and recvWindow)
   * @param timestamp The timestamp in milliseconds
   * @param recvWindow The receive window in milliseconds (optional, defaults to 5000)
   * @param secretKey The secret key as ByteVector
   * @return The base64-encoded signature
   */
  def signWithTimestamp(
    queryString: String,
    timestamp: Long,
    recvWindow: Long = 5000L,
    secretKey: ByteVector
  ): String = {
    val fullQueryString = if (queryString.isEmpty) {
      s"timestamp=${timestamp}&recvWindow=${recvWindow}"
    } else {
      s"${queryString}&timestamp=${timestamp}&recvWindow=${recvWindow}"
    }
    
    sign(fullQueryString, secretKey)
  }

  /**
   * URL encode a string for use in query parameters.
   * 
   * @param value The value to encode
   * @return The URL-encoded value
   */
  def urlEncode(value: String): String = {
    URLEncoder.encode(value, StandardCharsets.UTF_8.toString)
  }

  /**
   * Build a query string from parameters.
   * 
   * @param params Map of parameter names to values
   * @return The encoded query string
   */
  def buildQueryString(params: Map[String, String]): String = {
    params
      .filter { case (_, value) => value.nonEmpty }
      .map { case (key, value) => s"${urlEncode(key)}=${urlEncode(value)}" }
      .mkString("&")
  }

  /**
   * Create a complete signed query string for Binance API requests.
   * 
   * @param params The query parameters
   * @param timestamp The timestamp in milliseconds
   * @param recvWindow The receive window in milliseconds
   * @param secretKey The secret key as ByteVector
   * @return The complete query string with signature
   */
  def createSignedQueryString(
    params: Map[String, String],
    timestamp: Long,
    recvWindow: Long = 5000L,
    secretKey: ByteVector
  ): String = {
    val baseQueryString = buildQueryString(params)
    val signature = signWithTimestamp(baseQueryString, timestamp, recvWindow, secretKey)
    
    if (baseQueryString.isEmpty) {
      s"timestamp=${timestamp}&recvWindow=${recvWindow}&signature=${signature}"
    } else {
      s"${baseQueryString}&timestamp=${timestamp}&recvWindow=${recvWindow}&signature=${signature}"
    }
  }
}
