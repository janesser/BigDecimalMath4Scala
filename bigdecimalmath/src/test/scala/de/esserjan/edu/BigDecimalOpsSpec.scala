package de.esserjan.edu

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import BigDecimalMath._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigDecimalOpsSpec extends FlatSpec with Matchers with PropertyChecks {

  "BigDecimalOps" should "neg" in {
    forAll { (d: BigDecimal) =>
      val neg = d.neg
      d.abs should be(neg.abs)
      d.precision should be(neg.precision)
      if (d.signum == neg.signum)
        neg.signum should be(0)
      else
        d.signum * neg.signum should be(-1)
    }
  }

  it should "inv" in {
    forAll {
      (d: BigDecimal) =>
        val mc = new java.math.MathContext(d.precision)
        val zero = BigDecimal(0, 0, mc)
        val one = BigDecimal(1.0, mc)

        try {
          if (d.invertible) {
            val inv = d.inv

            inv.precision should be(d.precision)
            (inv * d) should be(one +- d.ulp)
          } else {
            an[ArithmeticException] should be thrownBy {
              zero.inv
            }
          }
        } catch {
          case ex: ArithmeticException =>
            println(s"Test omitted ($d.inv) because caught ArithmeticException: " + ex.getMessage)
        }
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