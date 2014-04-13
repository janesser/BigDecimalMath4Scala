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
   * calculation test FIXME
   */
  ignore should "provide equal E rounded and calculated" in {
    forAll(posIntsUpToEPrecision) {
      (precision: Int) =>
        whenever(precision > 20) {
          val mc = new java.math.MathContext(precision)
          
          val Ecalc = exp(BigDecimal(1.0, mc).setScale(precision+1))
          val Erounded = e(precision)
          
          Erounded.precision should be(precision)
          Ecalc.precision should be(precision)
          Ecalc should be(Erounded)
        }
    }
  }
}