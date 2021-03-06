package de.esserjan.scala.bigdecimal

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object BigDecimalAnalysisSpec {
  import de.esserjan.scala.bigdecimal.BigDecimalAnalysis._

  val posIntsUpToEPrecision =
    for (n <- Gen.choose(2, E_PRECISION)) yield n
}

@RunWith(classOf[JUnitRunner])
class BigDecimalAnalysisSpec extends FlatSpec with Matchers with PropertyChecks {
  import de.esserjan.scala.bigdecimal.BigDecimalAnalysis._
  import de.esserjan.scala.BigDecimalMath._

  import BigDecimalAnalysisSpec._

  "BigDecimalAnalysis" should "square-root big decimals" in {
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

  it should "provide E in every (available) precision" in {
    forAll(posIntsUpToEPrecision) {
      (precision: Int) =>
        e(precision).precision should be(precision)
    }
  }

  it should "provide E in unavailable precision" in {
    val E = e(E_PRECISION + 1)
    E.precision should be(E_PRECISION + 1)
  }

  /**
   * Calculation at point 1.0 with varying precision.
   *
   * Omit this test to keep a responsive test-set.
   */
  ignore should "provide equal E rounded and calculated" in {
    forAll(posIntsUpToEPrecision) {
      (precision: Int) =>
        whenever(precision > 1) {
          val mc = new java.math.MathContext(precision)

          val x = BigDecimal(1.0, mc).setScale(precision - 1)
          val Ecalc = exp(x)
          val Erounded = e(precision)

          val error = Ecalc - Erounded
          val xUlp = x.ulp

          Erounded.precision should be(precision)
          Ecalc.precision should be(precision)
          error.abs should be <= x.ulp
        }
    }
  }

  it should "provide exp(x) for some x" in {
    val precision = 20
    val mc = new java.math.MathContext(precision)
    val exps =
      Table(
        ("x", "e"),
        (BigDecimal(-2.0, mc).setScale(precision - 1), e(mc).pow(-2).round(mc)),
        (BigDecimal(0.0, mc), BigDecimal(1.0, mc)),
        (BigDecimal(2.0, mc).setScale(precision - 1), e(E_PRECISION).pow(2).round(mc)))
    forAll(exps) {
      (x: BigDecimal, e: BigDecimal) =>
        val Ecalc = exp(x)
        val xUlp = x.ulp

        val error: BigDecimal = exp(x) - e
        error.abs should be <= x.ulp
    }
  }

  it should "provide ln(x) for some x" in {
    val precision = 20
    val mc = new java.math.MathContext(precision)
    val lns =
      Table(
        ("x", "ln"),
        (e(mc).pow(-2).round(mc),
          BigDecimal(-2.0, mc).setScale(precision - 1)),
        (BigDecimal(1.0, mc),
          BigDecimal(0.0, mc)),
        (e(E_PRECISION).pow(2).round(mc),
          BigDecimal(2.0, mc).setScale(precision - 1)))
    forAll(lns) {
      (x: BigDecimal, ln: BigDecimal) =>
        x.ln should be(ln +- x.ulp)
    }
  }

  it should "resolve ln(exp(x)) == x" in {
    val precision = 12
    val mc = new java.math.MathContext(precision, java.math.RoundingMode.HALF_UP)

    forAll(for (n <- Gen.choose(-1e-10, 1.0)) yield BigDecimal(n, mc)) {
      (x: BigDecimal) =>
        x.setScale(2 * precision).exp.ln should be(x +- x.ulp)
    }
  }
}