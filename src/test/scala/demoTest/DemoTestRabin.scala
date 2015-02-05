package demoTest

import encryption.{Rabin, RabinPrivateKey, RabinPublicKey}
import general.{DAB, General}
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
 * Created by kasonchan on 2/5/15.
 */
class DemoTestRabin extends FlatSpec with ShouldMatchers with General with Rabin {
  val RabinKey1 = Rabin_GenerateKeyPair(1357)
  val msg = "ABCDEFG"

  "Rabin1. Rabin_GenerateKeyPair(12)" should "= (publicKey(77), privateKey(7," +
    " 11), DAB(1, -3, 2)" in {
    Rabin_GenerateKeyPair(12) should
      be((RabinPublicKey(77), RabinPrivateKey(7, 11), DAB(1, -3, 2)))
  }

  "Rabin2. Rabin_GenerateKeyPair(5925)" should "= (publicKey(34963469), " +
    "privateKey(5903, 5923), DAB(1, 2073, -2066)" in {
    Rabin_GenerateKeyPair(5925) should
      be((RabinPublicKey(34963469), RabinPrivateKey(5903, 5923), DAB(1, 2073, -2066)))
  }

  "Rabin3. Rabin_GenerateKeyPair(7060)" should "= (publicKey(49702451), " +
    "privateKey(7043, 7057), DAB(1, 504, -503))" in {
    Rabin_GenerateKeyPair(7060) should
      be((RabinPublicKey(49702451), RabinPrivateKey(7043, 7057), DAB(1, 504, -503)))
  }

  "Rabin4. encryptedMsg" should "= " + msg in {
    val encryptedMsg = Rabin_Encrypt(RabinKey1._1.n, msg)
    val decryptedMsg = Rabin_Decrypt(RabinKey1._2, RabinKey1._3, encryptedMsg)
    decryptedMsg should be(msg)
  }
}
