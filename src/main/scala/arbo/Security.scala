package arbo

import org.http4s.{Charset, UrlForm}

import scodec.bits.ByteVector

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object Security {

  def sign(once: Long, data: UrlForm, uri: String, privateKey: ByteVector): String  = {
    val algo = "HmacSHA512"
    val api_str = once.toString() + UrlForm.encodeString(Charset.`UTF-8`)(data)
    val api_256 = ByteVector(api_str.getBytes("UTF-8")).digest("SHA-256")
    val hmac = Mac.getInstance(algo)
    hmac.init(new SecretKeySpec(privateKey.toArray, algo))
    val sign = ByteVector(hmac.doFinal(uri.getBytes("UTF-8") ++ api_256.toArray))
    sign.toBase64
            }
}
