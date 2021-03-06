package de.esserjan.scala

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
    val precision = 32
    val mc = new java.math.MathContext(precision)

    val one = BigDecimal(1.0, mc)

    forAll(for (n <- Gen.choose(-1e32, 1e32)) yield BigDecimal(n, mc)) {
      (x: BigDecimal) =>
        try {
          val inv = x.inv

          (inv * x) should be(one +- x.ulp)
        } catch {
          case ex: ArithmeticException =>
            println(s"Test omitted ($x.inv) because caught ArithmeticException: " + ex.getMessage)
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