package de.esserjan.edu

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

import BigDecimalMath._

object BigDecimalOpsSpec {
  case class Square(val d: BigDecimal) {
    val sq = d * d
  }
  val squares = for (d <- Gen.choose(1.0, 1e32)) yield Square(d)

  val bigDecimals = for (n <- Gen.choose(-1e64, 1e64)) yield BigDecimal(n)
}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigDecimalOpsSpec extends FlatSpec with Matchers with PropertyChecks {
  import BigDecimalOpsSpec._
  
  "BigDecimalMath - BigDecimalOps" should "root squares" in {
    forAll(squares) { (sq: Square) =>
      sq.sq.sqrt should be(sq.d +- BigDecimal(10).pow(sq.sq.precision))
    }
  }

  it should "square-root anything" in {
    forAll(bigDecimals) {
      (s: BigDecimal) =>
        (s * s).sqrt should be(s.abs)
    }
  }

  it should "cubic-root anything" in {
    forAll(bigDecimals) {
      (s: BigDecimal) =>
        (s * s * s).cqrt should be(s)
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

  it should "faculty throw IllegalArgumentException" in {
    evaluating {
      BigDecimal(0.5).factorial
    } should produce[ArithmeticException]
  }
}