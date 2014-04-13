package de.esserjan.edu

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TrigonometrySpec extends FlatSpec with Matchers with PropertyChecks {
  import BigDecimalMath._
  import bigdecimal.BigDecimalTrigonometry._

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
        evaluating {
          pi(mc)
        } should produce[NotImplementedError]
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
}