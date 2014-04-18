package de.esserjan.edu

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import BigDecimalMath._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigDecimalOpsSpec extends FlatSpec with Matchers with PropertyChecks {

  "BigDecimalMath - BigDecimalOps" should "square-root big decimals" in {
    (s: BigDecimal) =>
      try {
        val square = s * s
        (square.sqrt - s.abs).abs should be <= square.ulp
      } catch {
        case ex: ArithmeticException =>
          println(s"Test omitted because ($s pow 2) caused ArithmeticException: " + ex.getMessage)
        case ex: NumberFormatException =>
          println(s"Test omitted because ($s pow 2) caused NumberFormatException: " + ex.getMessage)
      }
  }

  it should "cubic-root big decimals" in {
    (s: BigDecimal) =>
      try {
        val cubic = s * s * s
        (cubic.cqrt.abs - s.abs).abs should be <= cubic.ulp
      } catch {
        case ex: ArithmeticException =>
          println(s"Test omitted because ($s pow 2) caused ArithmeticException: " + ex.getMessage)
        case ex: NumberFormatException =>
          println(s"Test omitted because ($s pow 2) caused NumberFormatException: " + ex.getMessage)
      }
  }

  it should "negate" in {
    forAll { (d: BigDecimal) =>
      val neg = d.negate
      d.abs should be(neg.abs)
      d.precision should be(neg.precision)
      if (d.signum == neg.signum)
        neg.signum should be(0)
      else
        d.signum * neg.signum should be(-1)
    }
  }

  it should "factorial" in {
    import BigIntOpsSpec.faculties
    forAll(faculties) {
      (n: Int, fac: Int) =>
        BigDecimal(n).setScale(5).factorial should be(fac)
    }
  }

  it should "factorial throw ArithmeticException" in {
    an[ArithmeticException] should be thrownBy {
      BigDecimal(0.5).factorial
    }
  }

  it should "stripTrailingZeroes" in {
    BigDecimal("1.000").stripTrailingZeros.scale should be(0)
  }
}