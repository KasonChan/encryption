package demoTest

import demo.Demo.{gcd, pickE}
import org.scalatest.{ShouldMatchers, FlatSpec}

/**
 * Created by kasonchan on 1/25/15.
 */
class DemoTest extends FlatSpec with ShouldMatchers {
  "gcd(1, 192)" should "= 1" in {
    gcd(1, 192) should be(1)
  }

  "gcd(49, 192)" should "= 1" in {
    gcd(49, 192) should be(1)
  }

  "gcd(139, 192)" should "= 1" in {
    gcd(139, 192) should be(1)
  }

  "gcd(173, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }

  "gcd(191, 192)" should "= 1" in {
    gcd(173, 192) should be(1)
  }
}
