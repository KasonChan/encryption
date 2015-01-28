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

  val keys = RSA_GenerateKeyPair()
  val keys2 = RSA_GenerateKeyPair()

  "RSA decryptedB" should "= \"3as\" after encryption and decryption" in {
    val b = token("3as")
    val encryptedB = RSA_Encrypt(keys._1.x, keys._1.n, b.x)
    val decryptedB = RSA_Decrypt(keys._2.x, keys._2.n, encryptedB)
    decryptedB should be("3as")
  }

  "RSA decryptedC + 7" should "= 12 after encryption and decryption" in {
    val c = token(5.toString)
    val encryptedC = RSA_Encrypt(keys._1.x, keys._1.n, c.x)
    val decryptedC = RSA_Decrypt(keys._2.x, keys._2.n, encryptedC)
    decryptedC.toInt + 7 should be(12)
  }

  "RSA decryptedD + 7" should "= 12 after encryption and decryption" in {
    val d = token(5.toString)
    val encryptedD1 = RSA_Encrypt(keys._1.x, keys._1.n, d.x)
    val encryptedD2 = RSA_Encrypt(keys2._1.x, keys2._1.n, encryptedD1)
    val decryptedD1 = RSA_Decrypt(keys2._2.x, keys2._2.n, encryptedD2)
    val decryptedD2 = RSA_Decrypt(keys._2.x, keys._2.n, decryptedD1)
    decryptedD2.toInt + 7 should be(12)
  }
}
