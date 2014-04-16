package de.esserjan.edu.bigdecimal

import scala.math.BigDecimal.int2bigDecimal

object BigDecimalTools {
  trait EvalSeriesGoodEnough extends Function4[java.math.MathContext, Int, BigDecimal, BigDecimal, Boolean]
  case class SinceIndexGoodEnough(val n: Int) extends EvalSeriesGoodEnough {
    def apply(mc: java.math.MathContext, k: Int, fkx: BigDecimal, acc: BigDecimal): Boolean = k > n
  }
  /**
   * Evaluate a series given by `f` at the point `x`.
   *
   * @param x evaluate at that point (precision-driver)
   * @param f function for the k's series-component
   * @param n limit for series-components
   * @return $$\Sum_0^{n - 1} f_k(x)$$
   */
  def evalSeries(x: BigDecimal,
    f: (Int) => (BigDecimal => BigDecimal),
    n: Int): BigDecimal = evalSeries(x, f, SinceIndexGoodEnough(n))
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
  case class NewtonArithmeticException(val x: BigDecimal, val ex: Throwable) extends NewtonException(x)
  /**
   * @param x current approximation point
   * @param fn approximation iterator
   * @param goodEnough function to indicate when the approximation is sufficiently precise.
   * @param i iterations counter (zero-based)
   * @param iLimit iterations limit (defaults to `min(10 * x.precision, 5000)`)
   * @return approximation point x once goodEnough
   * @throws
   * NewtonLimitException
   * 	if goodEnough isn't satisfied after `iLimit` iterations, approximation is aborted.
   *    One catcher should indicate what operation caused the overflow.
   *    The passed `x` can be used under some circumstances.
   * NewtonArithmeticException on [[ArithmeticException]]
   */
  def newton(
    x: BigDecimal,
    fn: BigDecimal => BigDecimal,
    goodEnough: BigDecimal => Boolean): BigDecimal = {
    import scala.math.min
    val iLimit: Int = min(2 * x.precision, 5000)

    @scala.annotation.tailrec
    def newtonInternal(x: BigDecimal, i: Int = 0): BigDecimal =
      if (goodEnough(x)) x
      else if (i >= iLimit) throw NewtonLimitException(x, iLimit)
      else newtonInternal(fn(x), i + 1)

    try {
      newtonInternal(x)
    } catch {
      case ex: ArithmeticException =>
        throw NewtonArithmeticException(x, ex)
    }
  }
}