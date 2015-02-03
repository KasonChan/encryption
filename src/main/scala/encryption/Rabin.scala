package encryption

import general.{DAB, General}

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
  def Rabin_GenerateKeyPair(r: Int): (RabinPublicKey, RabinPrivateKey, DAB) = {
    val primes = generatePrimes(r)

    val p = primes(primes.length - 2)

    val q = primes(primes.length - 1)

    val n = p * q

    val DAB = extendedGcd(p, q)

    (RabinPublicKey(n), RabinPrivateKey(p, q), DAB)
  }

  def Rabin_Encrypt(p: Int, m: String): String = {
    val msg = stringToInt(m)

    val doubleMsg = msg.map(m => m.toBinaryString.concat(m.toBinaryString))

    val encryptedMsg = doubleMsg.map(m => Math.pow(Integer.parseInt(m, 2), 2) % p)

    doubleToString(encryptedMsg)
  }

  def Rabin_Decrypt(privateKey: RabinPrivateKey, dab: DAB, m: String) = {
    val msg = stringToInt(m)

    val p = privateKey.p
    val q = privateKey.q
    val n = p * q

    val r = msg.map(m => (Math.pow(m, 2) % p))
    val s = msg.map(m => (Math.pow(m, 2) % q))

    val xaps = s.map(s => dab.a * p * s)
    val xbqr = r.map(r => dab.b * q * r)
    val yaps = s.map(s => dab.a * p * s)
    val ybqr = r.map(r => dab.b * q * r)
    val xs = (xaps, xbqr).zipped.map(_ + _)
    val ys = (yaps, ybqr).zipped.map(_ - _)

    val x = xs.map(x => mod(x.toInt, n))
    val y = ys.map(y => mod(y.toInt, n))
    val nx = x.map(x => mod(-x.toInt, n))
    val ny = y.map(y => mod(-y.toInt, n))

    val m1 = x.map(x => x.toBinaryString)
    val m2 = nx.map(nx => List.fill(6 - nx.toBinaryString.length)("0").mkString + nx.toBinaryString)
    val m3 = y.map(y => y.toBinaryString)
    val m4 = ny.map(ny => ny.toBinaryString)

    //    println(m1 + " " + m2 + " " + m3 + " " + m4)
    //    TODO: Add test cases
  }

  def isHalfEqual(m: String): Boolean = {
    val m1 = m.substring(0, (m.length / 2))
    val m2 = m.substring((m.length / 2), m.length)

    if (m1 == m2)
      true
    else
      false
  }

}
