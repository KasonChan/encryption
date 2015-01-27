package demoTest

case class token(x: String)

import encryption.RSA
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
 * Created by kasonchan on 1/25/15.
 */
class DemoTest extends FlatSpec with ShouldMatchers with RSA {
  "gcd(1, 192)" should "= 1" in {
    gcd(1, 192) should be(1)
  }

  "gcd(49, 192)" should "= 1" in {
    gcd(49, 192) should be(1)
  }

  "gcd(139, 192)" should "= 1" in {
    gcd(139, 192) should be(1)
  }

  "gcd(173, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }

  "gcd(191, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }

  val keys = generateKeyPair()

  "decryptedB" should "= \"3as\" after encryption and decryption" in {
    val b = token("3as")
    val encryptedB = encrypt(keys._1.x, keys._1.n, b.x)
    val decryptedB = decrypt(keys._2.x, keys._2.n, encryptedB)
    decryptedB should be("3as")
  }

  "decryptedC + 7" should "= 12 after encryption and decryption" in {
    val c = token(5.toString)
    val encryptedC = encrypt(keys._1.x, keys._1.n, c.x)
    val decryptedC = decrypt(keys._2.x, keys._2.n, encryptedC)
    decryptedC.toInt + 7 should be(12)
  }
}
