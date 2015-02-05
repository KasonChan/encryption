package demoTest

import encryption.RSA
import general.General
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
 * Created by kasonchan on 2/5/15.
 */
class DemoTestRSA extends FlatSpec with ShouldMatchers with General with RSA {
  val RSA_Key = RSA_GenerateKeyPair(13)
  val RSA_Key2 = RSA_GenerateKeyPair(160)

  "RSA1. RSA decryptedB" should "= \"3as\" after encryption and " + "decryption" in {
    val b = token("3as")
    val encryptedB = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, b.x)
    val decryptedB = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, encryptedB)
    decryptedB should be("3as")
  }

  "RSA2. RSA decryptedC + 7" should "= 12 after encryption and decryption" in {
    val c = token(5.toString)
    val encryptedC = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, c.x)
    val decryptedC = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, encryptedC)
    decryptedC.toInt + 7 should be(12)
  }

  "RSA3. RSA decryptedD + 7" should "= 12 after encryption and decryption" in {
    val d = token(5.toString)
    val encryptedD1 = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, d.x)
    val encryptedD2 = RSA_Encrypt(RSA_Key2._1.x, RSA_Key2._1.n, encryptedD1)
    val decryptedD1 = RSA_Decrypt(RSA_Key2._2.x, RSA_Key2._2.n, encryptedD2)
    val decryptedD2 = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, decryptedD1)
    decryptedD2.toInt + 7 should be(12)
  }

  "RSA4. RSA decryptedE " should
    "= ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890 after encryption and decryption" in {
    val d = token("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
    val encryptedE1 = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, d.x)
    val encryptedE2 = RSA_Encrypt(RSA_Key2._1.x, RSA_Key2._1.n, encryptedE1)
    val decryptedE1 = RSA_Decrypt(RSA_Key2._2.x, RSA_Key2._2.n, encryptedE2)
    val decryptedE2 = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, decryptedE1)
    decryptedE2 should be("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
  }
}
