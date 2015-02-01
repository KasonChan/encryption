package general

/**
 * Created by kasonchan on 1/28/15.
 */
trait General {
  /**
   * Returns the greatest common divisor of a and b *
   * @param a Integer
   * @param b Integer
   * @return gcd(a, b) Integer
   */
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)


  /**
   * Returns d = gcd(a, b) and integers x, y satisfying ax + by = d *
   * @param a Integer
   * @param b Integer
   * @return (d: Int, x: Int, y: Int)
   */
  def extendedGcd(a: Int, b: Int): (Int, Int, Int) = {
    if (b == 0)
      (a, 1, 0)

    else {
      val r: (Int, Int, Int) = extendedGcd(b, a % b)
      val d = r._1;
      val x = r._3;
      val y = r._2 - (a / b) * r._3

      (d, x, y)
    }
  }

  /**
   * Returns a list of prime numbers *
   * @param n Integer
   */
  def generatePrimes(n: Int) = {
    val list = (1 to n).toList

    val list2 = list.filterNot(l => (l != 2) && (l % 2 == 0))

    val primes = multiplesFilter(2, n, list2)

    primes
  }

  /**
   * Returns a list of prime numbers*
   * @param x filtered prime number: Integer
   * @param n Integer
   * @param list List[Integer]
   * @return prime numbers: List[Integer]
   */
  def multiplesFilter(x: Int, n: Int, list: List[Int]): List[Int] = {
    if (x < Math.sqrt(n).ceil)
      multiplesFilter(x + 1, n, list.filterNot(l => (l != list(x)) && (l % list(x) == 0)))
    else list
  }

  /**
   * Converts a string m to a list of BigInt *
   * @param m String
   * @return List[BigInt]
   */
  def stringToBigInt(m: String): List[BigInt] = {
    m.map(m => BigInt(m)).toList
  }

  /**
   * Converts a string m to a list of Int * 
   * @param m String
   * @return List[Int]
   */
  def stringToInt(m: String): List[Int] = {
    m.map(m => m.toInt).toList
  }

  /**
   * Convert a list of Int to String * 
   * @param l List[Int]
   * @return String
   */
  def intToString(l: List[Int]): String = {
    (l.map(m => m.toChar)).mkString
  }

  /**
   * Convert a list of Double to String *
   * @param l List[Double]
   * @return String
   */
  def doubleToString(l: List[Double]): String = {
    (l.map(m => m.toInt.toChar)).mkString
  }

  /**
   * Convert a list of BigInt to String *
   * @param l List[BigInt]
   * @return String
   */
  def bigIntToString(l: List[BigInt]): String = {
    (l.map(m => m.toChar)).mkString
  }
}