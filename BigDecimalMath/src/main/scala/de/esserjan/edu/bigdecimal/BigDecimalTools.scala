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
    if (k < n) evalSeries(x, fx, n, k + 1, acc + fx(k)(x))
    else acc.round(new java.math.MathContext(x.precision))
  }

  case class IterationLimitException(val x: BigDecimal, val iLimit: Int) extends Exception
  val ITERATION_LIMIT = 1000
  /**
   * @param x current approximation point
   * @param fn approximation iterator
   * @param goodEnough function to indicate when the approximation is sufficiently precise.
   * @param i iterations counter (zero-based)
   * @param iLimit iterations limit (defaults to [[BigDecimalTools#ITERATION_LIMIT]])
   * @return approximation point x once goodEnough
   * @throws IterationLimitException
   * 	if goodEnough isn't satisfied after `iLimit` iterations, approximation is aborted.
   *    One catcher should indicate what operation caused the overflow.
   *    The passed `x` can be used under some circumstances.
   */
  @scala.annotation.tailrec
  def newton(
    x: BigDecimal,
    fn: BigDecimal => BigDecimal,
    goodEnough: BigDecimal => Boolean,
    i: Int = 0, iLimit: Int = ITERATION_LIMIT): BigDecimal =
    if (goodEnough(x)) x
    else if (i >= iLimit) throw IterationLimitException(x, iLimit)
    else newton(fn(x), fn, goodEnough, i + 1, iLimit)
}