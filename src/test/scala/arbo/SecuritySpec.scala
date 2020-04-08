package arbo

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.http4s.UrlForm
import scodec.bits.ByteVector

class SecuritySpec extends Specification {
  def is = s2"""
    Signs requests for Kraken $sign
"""

  def sign: MatchResult[String] = {
    val once = 1L
    val data = UrlForm("push" -> "yes")
    val uri = "/0/test"
    val key = "C73oXglF6eCmAvh8WMNU5TRDWSrPcZbmzvCY9BVy59eY91Lk6bIz8AQTJrDDbx6UGkjSwcc2EzIzRWeH7Z2psA=="
    val sign = Security.sign(once, data, uri, ByteVector.fromBase64(key).get)

    sign must beEqualTo("gjvvgRC6iDVWWQls+M/+iZdAXwr51gb8b770P14niH1k9yHmFIt7USv2oBmh15stbE/lI9wbfkmVTxFyquX76Q==")
  }

}
