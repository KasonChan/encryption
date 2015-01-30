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
   * Returns a list of prime numbers *
   * @param n Integer
   */
  def generatePrimes(n: Int) = {
    val list = (1 to n).toList
    
    val list2 = list.filterNot(l => (l != 2) && (l % 2 == 0))

    val list3 =  multiplesFilter(2, n, list2)
    
    list3
  }

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