package de.esserjan.edu.bigdecimal

import scala.math.BigDecimal.int2bigDecimal

object BigDecimalTools {
  /**
   * Evaluate a series given by `fx` at the point `x`.
   *
   * @param x evaluate at that point (precision-driver)
   * @param fx function for the k's series-component
   * @param n limit for series-components
   * @param k current series-component's index (starts with 0)
   * @param acc accumulator
   * @return `acc` once `k == n`
   */
  @scala.annotation.tailrec
  def evalSeries(x: BigDecimal,
    fx: (Int) => (BigDecimal => BigDecimal),
    n: Int, k: Int = 0, acc: BigDecimal = 0): BigDecimal = {
    if (k < n) evalSeries(x, fx, n, k + 1, fx(k)(x) + acc)
    else acc.round(new java.math.MathContext(x.precision))
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