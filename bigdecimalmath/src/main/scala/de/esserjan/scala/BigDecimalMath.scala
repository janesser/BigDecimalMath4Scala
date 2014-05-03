package de.esserjan.scala

/**
 * [[BigDecimalMath]] contains implicit classes [[BigIntOps]] and [[BigDecimalOps]] to extend their functionality.
 */
object BigDecimalMath {
  import bigdecimal.BigDecimalTools._
  import bigdecimal.BigDecimalTrigonometry._
  import bigdecimal.BigDecimalAnalysis._

  /**
   * [[BigIntOps]] extends [[scala.math.BigInt]] to have a factorial method.
   */
  implicit class BigIntOps(x: BigInt) {
    private[this] val MINUS_ONE = BigInt(-1)
    private[this] val ZERO = BigInt(0)
    private[this] val ONE = BigInt(1)

    def neg = x * MINUS_ONE

    def toBigDecimal(mc: java.math.MathContext) = BigDecimal(x, mc)

    /**
     * Ideomatic tail-recursive implementation of the factorial function:
     *
     * `fac(0) = 1`
     *
     * `fac(1) = 1`
     *
     * `fac(n) = n * fac(n-1)`
     *
     * `fac(-n) = -fac(n)`
     *
     * @return fac(x)
     */
    def factorial: BigInt = {
      @scala.annotation.tailrec
      def fac(n: BigInt, acc: BigInt = ONE): BigInt = {
        if (n == ZERO || n == ONE) acc
        else if (n < ZERO) fac(n.neg, acc.neg)
        else fac(n - 1, acc * n)
      }
      fac(x)
    }
  }

  /**
   * All operations are computed with the precision/scale of given `x`.
   * Every result should be approximately `error(func(x)) +- x.ulp ~ 0` if no other approximation is given.
   * @param x the wrapped [[scala.math.BigDecimal]]
   */
  implicit class BigDecimalOps(x: BigDecimal) {
    private[this] val mc = new java.math.MathContext(x.precision)
    private[this] val MINUS_ONE: BigDecimal = new java.math.BigDecimal(-1.0, mc)
    private[this] val ZERO: BigDecimal = new java.math.BigDecimal(0.0, mc)
    private[this] val ONE: BigDecimal = new java.math.BigDecimal(1.0, mc)

    /**
     * Additive inverse of `x` (may cause [[java.math.BigDecimal#inflate()]])
     *
     * @return `-x`
     */
    def neg = x * MINUS_ONE

    /**
     * `x` is invertible (and its inverse has a [[BigDecimal]]-representation)
     * if it is non-zero and has limited scale <= 1024.
     *
     * @return `true` if x is invertible.
     */
    def invertible: Boolean = x != ZERO && Range(1, 1024).isDefinedAt(x.scale)
    /**
     * Multiplicative inverse of `x` as long as the inverse exists and is representable.
     * 
     * @return `x^{-1}
     * @throws ArithmeticException on non-invertible `x``
     */
    def inv =
      if (!invertible)
        throw new ArithmeticException(s"$x is not invertible")
      else x.pow(-1).round(mc)

    /**
     * The factorial of `x`.
     *
     * @return `fac(x)`
     * @throws ArithmeticException if `x` is not an integer.
     */
    def factorial: BigDecimal =
      if (!x.isWhole) throw new ArithmeticException
      else x.toBigInt.factorial.toBigDecimal(mc)

    /**
     * The square-root of `x`.
     *
     * @return x^(1/2)
     */
    def sqrt = root(x, 2)
    /**
     * The cubic-root of `x`
     *
     * @return `x^(1/3)`
     */
    def cqrt = root(x, 3)

    /**
     * The sinus function evaluated at `x`.
     *
     * @return `sin(x)`
     */
    def sin = sinus(x)
    /**
     * The cosinus function evaluated at `x`.
     *
     * @return `cos(x) == sin(x + PI/2)`
     */
    def cos = cosinus(x)
    /**
     * The tangential function evaluated at `x`.
     *
     * @return `tan(x) == sin(x) / cos(x)
     */
    def tan = sin / cos

    /**
     * The exponential function evaluated at `x`.
     *
     * @return `exp(x)`
     */
    def exp = bigdecimal.BigDecimalAnalysis.exp(x)
    /**
     * The (natural) logarithm of `x`
     *
     * @return `ln(x)`
     */
    def ln = bigdecimal.BigDecimalAnalysis.ln(x)

    /**
     * @return `x` with stripped trailing zeroes.
     */
    def stripTrailingZeros = BigDecimal(x.underlying.stripTrailingZeros(), mc)
  }
}