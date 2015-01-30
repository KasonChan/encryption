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
  
  "generatePrimes(175)" should "= List(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173)" in {
    generatePrimes(175) should be(List(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173))
  }

  val list1 = (65 to 90).toList
  val list2 = list1.map(i => i.toDouble)
  
  val list3 = (97 to 122).toList
  val list4 = list3.map(i => i.toDouble)
  
  "intToString(list1)" should "= list1" in {
    intToString(list1) should be("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }
  
  "intToString(list2)" should "= list2" in {
    doubleToString(list2) should be("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }
  
  "intToString(list3)" should "= list3" in {
    intToString(list3) should be("abcdefghijklmnopqrstuvwxyz")
  }
  
  "intToString(list4)" should "= list4" in {
    doubleToString(list4) should be("abcdefghijklmnopqrstuvwxyz")
  }

  val RSA_Key = RSA_GenerateKeyPair()
  val RSA_Key2 = RSA_GenerateKeyPair()

  "RSA decryptedB" should "= \"3as\" after encryption and decryption" in {
    val b = token("3as")
    val encryptedB = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, b.x)
    val decryptedB = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, encryptedB)
    decryptedB should be("3as")
  }

  "RSA decryptedC + 7" should "= 12 after encryption and decryption" in {
    val c = token(5.toString)
    val encryptedC = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, c.x)
    val decryptedC = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, encryptedC)
    decryptedC.toInt + 7 should be(12)
  }

  "RSA decryptedD + 7" should "= 12 after encryption and decryption" in {
    val d = token(5.toString)
    val encryptedD1 = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, d.x)
    val encryptedD2 = RSA_Encrypt(RSA_Key2._1.x, RSA_Key2._1.n, encryptedD1)
    val decryptedD1 = RSA_Decrypt(RSA_Key2._2.x, RSA_Key2._2.n, encryptedD2)
    val decryptedD2 = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, decryptedD1)
    decryptedD2.toInt + 7 should be(12)
  }

  "RSA decryptedE " should 
    "= ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890 after encryption and decryption" in {
    val d = token("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
    val encryptedE1 = RSA_Encrypt(RSA_Key._1.x, RSA_Key._1.n, d.x)
    val encryptedE2 = RSA_Encrypt(RSA_Key2._1.x, RSA_Key2._1.n, encryptedE1)
    val decryptedE1 = RSA_Decrypt(RSA_Key2._2.x, RSA_Key2._2.n, encryptedE2)
    val decryptedE2 = RSA_Decrypt(RSA_Key._2.x, RSA_Key._2.n, decryptedE1)
    decryptedE2 should be("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
  }
}
