package demoTest

case class token(x: String)

import general.{DAB, General}
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
 * Created by kasonchan on 1/25/15.
 */
class DemoTest extends FlatSpec with ShouldMatchers with General {
  "1. gcd(1, 192)" should "= 1" in {
    gcd(1, 192) should be(1)
  }

  "2. gcd(49, 192)" should "= 1" in {
    gcd(49, 192) should be(1)
  }

  "3. gcd(139, 192)" should "= 1" in {
    gcd(139, 192) should be(1)
  }

  "4. gcd(173, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }

  "5. gcd(191, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }

  "6. extendedGcd(4864, 3458)" should "= (38, 32, -45)" in {
    extendedGcd(4864, 3458) should be(DAB(38, 32, -45))
  }

  "7. extendedGcd(65, 40)" should "= (5, -3, 5)" in {
    extendedGcd(65, 40) should be(DAB(5, -3, 5))
  }

  "8. extendedGcd(1239, 735)" should "= (21, -16, 27)" in {
    extendedGcd(1239, 735) should be(DAB(21, -16, 27))
  }

  "9. generatePrimes(175)" should "= List(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173)" in {
    generatePrimes(175) should be(List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173))
  }

  val list1 = (65 to 90).toList
  val list2 = list1.map(i => i.toDouble)

  val list3 = (97 to 122).toList
  val list4 = list3.map(i => i.toDouble)

  "10. intToString(list1)" should "= list1" in {
    intToString(list1) should be("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }

  "11. intToString(list2)" should "= list2" in {
    doubleToString(list2) should be("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }

  "12. intToString(list3)" should "= list3" in {
    intToString(list3) should be("abcdefghijklmnopqrstuvwxyz")
  }

  "13. intToString(list4)" should "= list4" in {
    doubleToString(list4) should be("abcdefghijklmnopqrstuvwxyz")
  }

  "14. mod(0, 1000)" should "= 1000" in {
    mod(0, 1000) should be(0)
  }

  "15. mod(-0, 1000)" should "= 1000" in {
    mod(-0, 1000) should be(0)
  }

  "16. mod(-2000, 2000)" should "= 0" in {
    mod(-2000, 2000) should be(0)
  }

  "17. mod(2000, 2000)" should "= 0" in {
    mod(2000, 2000) should be(0)
  }

  "18. mod(-5, 1000)" should "= 995" in {
    mod(-5, 1000) should be(995)
  }

  "19. mod(5, 1000)" should "= 5" in {
    mod(5, 1000) should be(5)
  }

  "20. mod(-4321, 1234)" should "= 615" in {
    mod(-4321, 1234) should be(615)
  }

  "21. mod(4321, 1234)" should "= 619" in {
    mod(4321, 1234) should be(619)
  }

  "22. mod(2468, 1357)" should "= 1111" in {
    mod(2468, 1357) should be(1111)
  }

  "23. mod(-2468, 1357)" should "= 246" in {
    mod(-2468, 1357) should be(246)
  }

  "24. mod(2468, -1357)" should "= -246" in {
    mod(2468, -1357) should be(-246)
  }

  "25. mod(-2468, -1357)" should "= -1111" in {
    mod(-2468, -1357) should be(-1111)
  }

  "26. extendedGcd(540, 168)" should "= (12, 5, -16)" in {
    extendedGcd(540, 168) should be(DAB(12, 5, -16))
  }

  "27. residues(7)" should "= Seq(0,1,2,3,4,5,6)" in {
    residues(7) should be(Seq(0, 1, 2, 3, 4, 5, 6))
  }

  "28. congruence(7, 0, 21)" should "= Seq(-21,-14,-7,0,7,14,21)" in {
    congruence(7, 0, 21) should be(Seq(-21, -14, -7, 0, 7, 14, 21))
  }

  "29. congruence(7, 3, 21)" should "= Seq(-18,-11,-4,3,10,17)" in {
    congruence(7, 3, 21) should be(Seq(-18, -11, -4, 3, 10, 17))
  }

  "30. residueClass(7, 21)" should "= Seq(Seq(-21, -14, -7, 0, 7, 14, 21), " +
    "Seq(-20, -13, -6, 1, 8, 15), Seq(-19, -12, -5, 2, 9, 16)," +
    "Seq(-18, -11, -4, 3, 10, 17), Seq(-17, -10, -3, 4, 11, 18)," +
    "Seq(-16, -9, -2, 5, 12, 19), Seq(-15, -8, -1, 6, 13, 20))" in {
    residueClass(7, 21) should be(Seq(Seq(-21, -14, -7, 0, 7, 14, 21),
      Seq(-20, -13, -6, 1, 8, 15), Seq(-19, -12, -5, 2, 9, 16),
      Seq(-18, -11, -4, 3, 10, 17), Seq(-17, -10, -3, 4, 11, 18),
      Seq(-16, -9, -2, 5, 12, 19), Seq(-15, -8, -1, 6, 13, 20)))
  }

  "31. multiplicativeInverse(6)" should "= Some(Seq(1,5))" in {
    multiplicativeInverse(6) should be(Some(Seq(1, 5)))
  }

  "32. multiplicativeInverse(10)" should "= Some(Seq(1,3,7,9))" in {
    multiplicativeInverse(10) should be(Some(Seq(1, 3, 7, 9)))
  }

  "33. multiplicativeInverse(0)" should "= None" in {
    multiplicativeInverse(0) should be(None)
  }

  "34. multiplicativeInverse(192)" should "= Some(Seq(1, 5, 7, 11, 13, 17, " +
    "19, 23, 25, 29, 31, 35, 37, 41, 43, 47, 49, 53, 55, 59, 61, 65, 67, 71, " +
    "73, 77, 79, 83, 85, 89, 91, 95, 97, 101, 103, 107, 109, 113, 115, 119, " +
    "121, 125, 127, 131, 133, 137, 139, 143, 145, 149, 151, 155, 157, 161, " +
    "163, 167, 169, 173, 175, 179, 181, 185, 187, 191))" in {
    multiplicativeInverse(192) should be(Some(Seq(1, 5, 7, 11, 13, 17, 19,
      23, 25, 29, 31, 35, 37, 41, 43, 47, 49, 53, 55, 59, 61, 65, 67, 71, 73,
      77, 79, 83, 85, 89, 91, 95, 97, 101, 103, 107, 109, 113, 115, 119, 121,
      125, 127, 131, 133, 137, 139, 143, 145, 149, 151, 155, 157, 161, 163,
      167, 169, 173, 175, 179, 181, 185, 187, 191)))
  }
}
