package de.esserjan.edu.bigdecimal

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object BigDecimalAnalysisSpec {
  import de.esserjan.edu.bigdecimal.BigDecimalAnalysis._
  val posIntsUpToEPrecision =
    for (n <- Gen.choose(1, E_PRECISION)) yield n
}

@RunWith(classOf[JUnitRunner])
class BigDecimalAnalysisSpec extends FlatSpec with Matchers with PropertyChecks {
  import de.esserjan.edu.BigDecimalMath._
  import de.esserjan.edu.bigdecimal.BigDecimalAnalysis._

  import BigDecimalAnalysisSpec._

  "BigDecimalAnalysis" should "provide E in every (available) precision" in {
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
   * calculation at point 1.0 with varying precision
   */
  it should "provide equal E rounded and calculated" in {
    forAll(posIntsUpToEPrecision) {
      (precision: Int) =>
          println(s"precision: $precision")
          val mc = new java.math.MathContext(precision)

          val x = BigDecimal(1.0, mc).setScale(precision - 1)
          val Ecalc = exp(x)
          val Erounded = e(precision)

          val error = Ecalc - Erounded
          val xUlp = Ecalc.ulp
          println(s"Ecalc:  $Ecalc\nEroun:  $Erounded\nerror:  $error\nxUlp:   $xUlp")

          Erounded.precision should be(precision)
          Ecalc.precision should be(precision)
          error.abs should be <= x.ulp
    }
  }

  it should "provide exp(x) for some x" in {
    val precision = 2 * EXP_MIN_PRECISION
    val mc = new java.math.MathContext(precision)
    val exps =
      Table(
        ("x", "e"),
        (BigDecimal(-2.0, mc).setScale(precision - 1), e(mc).pow(-2).round(mc)),
        (BigDecimal(0.0, mc), BigDecimal(1.0)),
        (BigDecimal(2.0, mc).setScale(precision - 1), e(mc).pow(2).round(mc)))
    forAll(exps) {
      (x: BigDecimal, e: BigDecimal) =>
        val error: BigDecimal = exp(x) - e
        error.abs should be <= x.ulp
    }
  }
}