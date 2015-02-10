package encryption

import general.General

/**
 * Created by kasonchan on 1/24/15.
 */
case class key(x: Int, n: Int)

trait RSA extends General {
  /**
   * Returns a pair of private and public keys using p = 13, q = 17 *
   * @return (key(e, n), key(d, n))
   */
  def RSA_GenerateKeyPair(r: Int) = {
    val primes = generatePrimes(r)
    println("primes: " + primes)

    val p = primes.init.last
    println("p: " + p)

    val q = primes.last
    println("q: " + q)

    val n = p * q
    println("n: " + n)

    val phi = (p - 1) * (q - 1)
    println("phi: " + phi)

    val e = RSA_PickE(phi)
    println("e: " + e)

    val d = RSA_PickD(e, phi)
    println("d: " + d)

    (key(e, n), key(d, n))
  }

  /**
   * Returns a random integer e such that the gcd(phi, e) = 1 *
   * @param phi Integer
   * @return e Integer
   */
  def RSA_PickE(phi: Int): Int = {
    val es = for (i <- 1 until phi if (gcd(i, phi) == 1)) yield i
    println("es: " + es)

    es.last
  }

  /**
   * Returns a pair of e and d such that 1 < d < phi, and ed = 1 (mod phi) *
   * and e != d *
   * @param e Integer
   * @param phi Integer
   * @return ED(e: Int, d: Int)
   */
  def RSA_PickD(e: Int, phi: Int): Int = {
    val ds = (1 until phi).toList

    val phis = for {
      i <- 1 until phi
    } yield i * phi + 1
    println("phis: " + phis)

    val rs = for {
      phi <- phis
      d <- ds
      if (((e * d) == phi) && (phi % d == 0))
    } yield d
    println("rs: " + rs)

    rs.last
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
}
