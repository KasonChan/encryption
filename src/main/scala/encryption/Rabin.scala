package encryption

import general.General

case class RabinPublicKey(n: Int)

case class RabinPrivateKey(p: Int, q: Int)

/**
 * Created by kasonchan on 1/30/15.
 */
trait Rabin extends General {
  /**
   * Returns a pair of public key and private key * 
   * @return (public key n: Int, private key(p: Int, q: Int))
   */
  def Rabin_GenerateKeyPair(r: Int): (RabinPublicKey, RabinPrivateKey) = {
    val primes = generatePrimes(r)

    val p = primes(primes.length - 1)

    val q = primes(primes.length - 2)

    val n = p * q

    (RabinPublicKey(n), RabinPrivateKey(p, q))
  }

  def Rabin_Encrypt(p: Int, m: String): String = {
    val msg = stringToInt(m)

    val encryptedMsg = msg.map(m => (m * m) % p)

    intToString(encryptedMsg)
  }

  def Rabin_Decrypt(p: RabinPrivateKey, m: String) = {
    val msg = stringToInt(m)

    //    TODO: Finish up decryption
  }
}
