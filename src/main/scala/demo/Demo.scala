package demo

import encryption.RSA

/**
 * Created by kasonchan on 1/23/15.
 */
object Demo extends RSA {

  def main(args: Array[String]) {
    val keys = RSA_GenerateKeyPair(200)

    println(keys._1)
    println(keys._2)

    val msg = "Hello world"
    println(msg)

    val encryptedMsg = RSA_Encrypt(keys._1.x, keys._1.n, msg)

    val decryptedMsg = RSA_Decrypt(keys._2.x, keys._2.n, encryptedMsg)
    println("Decrypted message: " + decryptedMsg)
  }
}
