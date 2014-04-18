package de.esserjan.edu.bigdecimal

import scala.math.BigDecimal.int2bigDecimal

object BigDecimalTools {
  trait EvalSeriesGoodEnough extends Function4[java.math.MathContext, Int, BigDecimal, BigDecimal, Boolean]
  case class EvalSeriesSinceIndex(val n: Int) extends EvalSeriesGoodEnough {
    def apply(mc: java.math.MathContext, k: Int, fkx: BigDecimal, acc: BigDecimal): Boolean = k > n
  }
  case object EvalSeriesPrecise extends EvalSeriesGoodEnough {
    def apply(mc: java.math.MathContext, k: Int, fkx: BigDecimal, acc: BigDecimal) = (fkx + acc).round(mc) == acc.round(mc)
  }
  /**
   * Evaluate a series given by `f` at the point `x`.
   *
   * @param x evaluate at that point (precision-driver)
   * @param f function for the k's series-component
   * @param n limit for series-components
   * @return $$\Sum_0^n f_k(x)$$
   */
  def evalSeries(x: BigDecimal,
    f: (Int) => (BigDecimal => BigDecimal),
    n: Int): BigDecimal = evalSeries(x, f, EvalSeriesSinceIndex(n))
  def evalSeries(x: BigDecimal,
    f: (Int) => (BigDecimal => BigDecimal),
    goodEnough: EvalSeriesGoodEnough) = {
    val mc = new java.math.MathContext(x.precision)
    val zero = BigDecimal(0.0, mc)

    @scala.annotation.tailrec
    def seriesInternal(k: Int = 0, acc: BigDecimal = zero): BigDecimal = {
      val fk = f(k)
      val fkx = fk(x).apply(mc)
      if (goodEnough(mc, k, fkx, acc)) acc.round(mc)
      else seriesInternal(k + 1, fkx + acc)
    }

    seriesInternal()
  }

  sealed class NewtonException(lastResult: BigDecimal) extends Exception
  case class NewtonLimitException(val x: BigDecimal, val iLimit: Int) extends NewtonException(x)

  trait NewtonGoodEnough extends Function2[Int, BigDecimal, Boolean]
  case class NewtonGoodEnoughWrapper(goodEnough: (Int, BigDecimal) => Boolean) extends NewtonGoodEnough {
    def this(goodEnough: (BigDecimal) => Boolean) =
      this((i: Int, x: BigDecimal) => goodEnough(x))
    def apply(i: Int, x: BigDecimal): Boolean = goodEnough(i, x)
  }
  import scala.language.implicitConversions
  implicit def goodEnoughFunction1(goodEnough: (BigDecimal) => Boolean) = new NewtonGoodEnoughWrapper(goodEnough)
  implicit def goodEnoughFunction2(goodEnough: (Int, BigDecimal) => Boolean) = NewtonGoodEnoughWrapper(goodEnough)

  case class NewtonCombinedGoodEnough(ge: NewtonGoodEnough*) extends NewtonGoodEnough {
    def apply(i: Int, x: BigDecimal): Boolean = ge map { _(i, x) } reduce { (x, y) => x || y }
  }

  case class NewtonIterationLimit(iLimit: Int) extends NewtonGoodEnough {
    def apply(i: Int, x: BigDecimal): Boolean = {
      if (i > iLimit) throw NewtonLimitException(x, iLimit)
      else false
    }
  }
  case class NewtonResultPrecise(fn: BigDecimal => BigDecimal, precision: BigDecimal) extends NewtonGoodEnough {
    def apply(i: Int, x: BigDecimal): Boolean = {
      val mc = new java.math.MathContext(x.precision)

      val delta = x - fn(x)
      delta.abs <= precision
    }
  }

  /**
   * Newton-approximation, caller has to check for convergence.
   * Use [[NewtonIterationLimit]] in case of doubts.
   *
   * @param x current approximation point
   * @param fn approximation iterator
   * @param goodEnough function to indicate when the approximation is sufficiently precise.
   * @param i iterations counter (zero-based)
   * @return approximation point x once goodEnough
   * @throws NewtonException
   */
  def newton(
    x: BigDecimal,
    fn: BigDecimal => BigDecimal,
    goodEnough: NewtonGoodEnough): BigDecimal = {

    @scala.annotation.tailrec
    def newtonInternal(x: BigDecimal, i: Int = 0): BigDecimal =
      if (goodEnough(i, x)) x
      else newtonInternal(fn(x), i + 1)

    newtonInternal(x)
  }
}