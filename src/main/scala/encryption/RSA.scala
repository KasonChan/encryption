package encryption

import general.General

/**
 * Created by kasonchan on 1/24/15.
 */
case class ED(e: Int, d: Int)

case class key(x: Int, n: Int)

trait RSA extends General {
  /**
   * Returns a pair of private and public keys using p = 13, q = 17 *
   * @return (key(e, n), key(d, n))
   */
  def RSA_GenerateKeyPair(r: Int) = {
    val primes = generatePrimes(r)

    val p = primes.init.last

    val q = primes.last

    val n = p * q

    val phi = (p - 1) * (q - 1)

    val e = RSA_PickE(phi)

    val ed = RSA_PickED(e, phi)

    (key(ed.e, n), key(ed.d, n))
  }

  /**
   * Encrypts the message m with key (e, n) and returns the encrypted message *
   * @param e Integer
   * @param n Integer
   * @param m String
   * @return encryptedMsg List[BigInt]
   */
  def RSA_Encrypt(e: Int, n: Int, m: String): String = {
    val msg = stringToBigInt(m)

    val encryptedMsg = msg.map(m => (m.pow(e)) % n)

    bigIntToString(encryptedMsg)
  }

  /**
   * Decrypts the message m with the key (d, n) and returns the decrypted
   * message *
   * @param d Integer
   * @param n Integer
   * @param m List[BigInt]
   * @return decryptedMsg List[BigInt]
   */
  def RSA_Decrypt(d: Int, n: Int, m: String): String = {
    val msg = stringToBigInt(m)

    val decryptedMsg = msg.map(m => (m.pow(d)) % n)

    bigIntToString(decryptedMsg)
  }

  /**
   * Returns a random integer e such that the gcd(phi, e) = 1 *
   * @param phi Integer
   * @return e Integer
   */
  private def RSA_PickE(phi: Int): Int = {
    val es = for (i <- 1 to phi if (gcd(i, phi) == 1)) yield i

    val g = scala.util.Random

    val r = g.nextInt(es.length / 2)

    es(r + 1)
  }

  /**
   * Returns a pair of e and d such that 1 < d < phi, and ed = 1 (mod phi) *
   * and e != d *
   * @param e Integer
   * @param phi Integer
   * @return ED(e: Int, d: Int)
   */
  private def RSA_PickED(e: Int, phi: Int): ED = {
    val ds = for (i <- 1 to 1000) yield ((i * phi) + 1)

    val rs = for (i <- ds if ((i % e) == 0)) yield i

    val fs = rs.map(i => i / e)

    if (fs(0) == e) RSA_PickED(RSA_PickE(phi), phi)
    else ED(e, fs(0))
  }
}
