package demoTest

import encryption.ElGamal
import general.General
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
 * Created by kasonchan on 2/5/15.
 */
class DemoTestElGamal extends FlatSpec with ShouldMatchers with General with ElGamal {
  val ElGamalKey1 = ElGamal_GenerateKeyPair(2360)

  "ElGamal1. ElGamal decryptedMsg" should "= 128 after encryption and decryption" in {
    val m = 128
    val encryptedMsg = ElGamal_Encrypt(ElGamalKey1._1, m)
    val decryptedMsg = ElGamal_Decrypt(ElGamalKey1._1, ElGamalKey1._2, encryptedMsg)
    decryptedMsg should be(128)
  }

  "ElGamal2. ElGamal decryptedMsg" should "= 1 after encryption and decryption" in {
    val m = 1
    val encryptedMsg = ElGamal_Encrypt(ElGamalKey1._1, m)
    val decryptedMsg = ElGamal_Decrypt(ElGamalKey1._1, ElGamalKey1._2, encryptedMsg)
    decryptedMsg should be(1)
  }
}
