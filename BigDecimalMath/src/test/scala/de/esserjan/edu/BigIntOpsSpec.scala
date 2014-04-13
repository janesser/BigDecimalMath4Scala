package de.esserjan.edu

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import BigDecimalMath._

object BigIntOpsSpec {
  import org.scalatest.prop.PropertyChecks.Table
  val faculties = Table(
    ("n", "fac"),
    (0, 1),
    (1, 1),
    (6, 6 * 5 * 4 * 3 * 2 * 1),
    (-5, 5 * 4 * 3 * 2 * -1))
}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BigIntOpsSpec extends FlatSpec with Matchers with PropertyChecks {
  import BigIntOpsSpec._
  "BigDecimalMath - BigIntOps" should "negate" in {
    forAll { (i: BigInt) =>
      whenever(i != 0) {
        val neg = i.negate
        neg.abs should be(i.abs)
        neg.signum * i.signum should be(-1)
      }
    }
  }

  it should "enable BigIntOps - factorial" in {
    forAll(faculties) {
      (n: Int, fac: Int) =>
        BigInt(n).factorial.intValue should be(fac)
    }
  }
}