package de.esserjan.scala.bigdecimal

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigDecimalTrigonometrySpec extends FlatSpec with Matchers with PropertyChecks {
  import de.esserjan.scala.bigdecimal.BigDecimalTrigonometry._
  import de.esserjan.scala.BigDecimalMath._

  val PI: Double = java.lang.Math.PI

  val posInts = for (n <- Gen.choose(1, Int.MaxValue - 1)) yield n

  "BigDecimalMath - Trigonometry" should "deliver PI in appropriate precision" in {
    forAll(posInts) { (n: Int) =>
      val mc = new java.math.MathContext(n)
      if (n < PI_PRECISION) {
        pi(mc).precision should be(n)
      } else if (n == PI_PRECISION) {
        pi(mc).precision should be(PI_PRECISION)
      } else /*(n > PI_PRECISION)*/ {
        an[NotImplementedError] should be thrownBy {
          pi(mc)
        }
      }
    }
  }

  it should "deliver PI in best precision" in {
    val PI = pi(PI_PRECISION)
    PI.precision should be(PI_PRECISION)
  }

  val sinii = Table(
    ("x", "sin"),
    (-PI, 0.0),
    (0.0, 0.0),
    (PI, 0.0),
    (PI * 2.0, 0.0),
    (2.0 * PI, 0.0),
    (PI / 2.0, 1.0),
    (PI * 3.0 / 2.0, -1.0),
    (PI / 3, 0.866025),
    (PI * 4.0, 0.0))

  it should "enable trigonometry - sin" in {
    forAll(sinii) {
      (x: Double, sin: Double) =>
        val X = BigDecimal(x)
        val Sin = X.sin
        Sin should be(BigDecimal(sin) +- 0.1)
        if (X != 0 && Sin != 0)
          X.scale should be <= Sin.scale
    }
  }

  val cosinii = Table(
    ("x", "cos"),
    (-PI / 2.0, 0.0),
    (0.0, 1.0),
    (PI / 2.0, 0.0),
    (PI, -1.0),
    (PI * 3.0 / 2.0, 0.0),
    (PI / 3.0, 0.5),
    (PI * 2.0, 1.0),
    (PI * 4.0, 1.0))
  it should "enable trigonometry - cos" in {
    forAll(cosinii) {
      (x: Double, cos: Double) =>
        val X = BigDecimal(x)
        val Cos = X.cos
        Cos should be(BigDecimal(cos) +- 0.1)
        if (X != 0 && Cos != 0)
          X.scale should be <= Cos.scale
    }
  }

  it should "cos^2 + sin^2 = 1" in {
    val precision = 20
    val mc = new java.math.MathContext(precision)

    val one = BigDecimal(1.0, mc)

    forAll(for (n <- Gen.choose(0.0, 2.0 * java.lang.Math.PI)) yield BigDecimal(n, mc)) {
      (r: BigDecimal) =>
        // double precision of r.sin and r.cos
        val R = r.setScale(2 * precision)
        
        val cr = R.cos
        val sr = R.sin

        val cr2 = cr.pow(2)
        val sr2 = sr.pow(2)

        val sum = cr2 + sr2

        sum.round(mc) should be(one +- r.ulp)
    }

  }
}