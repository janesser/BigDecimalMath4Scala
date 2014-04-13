package de.esserjan.edu

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import BigDecimalMath._

object BigDecimalOpsSpec {
  case class Square(val d: BigDecimal) {
    val sq = d * d
  }
  /**
   * Range of squares (with positive roots).
   */
  val squares = for (d <- Gen.choose(1.0, 1e32)) yield Square(d)

  /**
   * Generic range of positive and negative BigDecimals.
   */
  val bigDecimals = for (n <- Gen.choose(-1e64, 1e64)) yield BigDecimal(n)
}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigDecimalOpsSpec extends FlatSpec with Matchers with PropertyChecks {
  import BigDecimalOpsSpec._

  "BigDecimalMath - BigDecimalOps" should "root squares" in {
    forAll(squares) { (sq: Square) =>
      sq.sq.sqrt should be(sq.d)
    }
  }

  def testSquare = (s: BigDecimal) =>
    try {
      (s * s).sqrt should be(s.abs +- s.ulp)
    } catch {
      case ex: ArithmeticException =>
        println(s"Test omitted because ($s pow 2) caused ArithmeticException: " + ex.getMessage)
      case ex: NumberFormatException =>
        println(s"Test omitted because ($s pow 2) caused NumberFormatException: " + ex.getMessage)
    }

  it should "square-root some bigDecimals" in {
    forAll(bigDecimals)(testSquare)
  }

  it should "square-root +-1E150" in {
    forAll(for (n <- Gen.choose(-1e150, 1e150)) yield BigDecimal(n))(testSquare)
  }
  
  it should "square-root [0,1E-150]" in {
    forAll(for (n <- Gen.choose(0.0, 1e-150)) yield BigDecimal(n))(testSquare)
  } 

  it should "cubic-root some bigDecimals" in {
    forAll(bigDecimals) {
      (s: BigDecimal) =>
        (s * s * s).cqrt should be(s +- s.ulp)
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
        BigDecimal(n).factorial.intValue should be(fac)
    }
  }

  it should "factorial throw ArithmeticException" in {
    evaluating {
      BigDecimal(0.5).factorial
    } should produce[ArithmeticException]
  }

  it should "stripTrailingZeroes" in {
    forAll(bigDecimals) {
      (d: BigDecimal) =>
        val s = d.setScale(d.scale + 3)

        s.stripTrailingZeros.scale should be(d.scale)
    }
  }
}