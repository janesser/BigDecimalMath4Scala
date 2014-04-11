package de.esserjan.edu

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.NotImplementedError

@RunWith(classOf[JUnitRunner])
class BigDecimalMathSpec extends FlatSpec with Matchers with PropertyChecks {

  import BigDecimalMath._
  import org.scalacheck.Gen

  val PI = java.lang.Math.PI

  val posInts = for (n <- Gen.choose(1, Int.MaxValue - 1)) yield n

  "BigDecimalMath" should "deliver PI in appropriate precision" in {
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

  case class Square(val d: BigDecimal) {
    val sq = d * d
  }
  
  val squares = for (d <- Gen.choose(1.0, 1e16)) yield Square(d)

  it should "root squares" in {
    forAll(squares) { (sq: Square) =>
      sqrt(sq.sq) should be(sq.d +- BigDecimal(10).pow(sq.sq.precision))
    }
  }

  it should "enable BigIntOps - negate" in {
    forAll { (i: BigInt) =>
      whenever(i != 0) {
        val neg = i.negate
        neg.abs should be(i.abs)
        neg.signum * i.signum should be(-1)
      }
    }
  }

  val faculties = Table(
    ("n", "fac"),
    (0, 1),
    (1, 1),
    (6, 6 * 5 * 4 * 3 * 2 * 1),
    (-5, 5 * 4 * 3 * 2 * -1))
    
  it should "enable BigIntOps - factorial" in {
    forAll(faculties) {
      (n: Int, fac: Int) =>
        BigInt(n).factorial.intValue should be(fac)
    }
  }

  it should "enable BigDecimalOps - negate" in {
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

  it should "enable BigDecimalOps - factorial" in {
    forAll(faculties) {
      (n: Int, fac: Int) =>
        BigDecimal(n).factorial.intValue should be(fac)
    }
  }

  it should "enable BigDecimalOps - faculty throw IllegalArgumentException" in {
    evaluating {
      BigDecimal(0.5).factorial
    } should produce[IllegalArgumentException]
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