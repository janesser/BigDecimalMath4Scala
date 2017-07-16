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
   * 
   * 
   * [[java.lang.OutOfMemoryError]] may occur for some edge cases, like e.g. -1E123456 or 1E-123456.
   * Within computations the scale is maintained stable. Almost every computation will cause `x` to inflate.
   *
   * Every result should be approximately `error(func(x)) +- x.ulp ~ 0` if no other approximation is given.
   * See corresponding [[PropertySpec]] for more detail.
   *
   *
   * Properties that require double precision to match above result quality:
   *
   * `x.ln.exp == x.ln.exp`
   *
   * `x * x.inv == 1`
   *
   * `x.cos.pow(2) + x.sin.pow(2) == 1`
   *
   * TBC
   *
   * @param [[scala.math.BigDecimal]] x
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
     * Multiplicative inverse of `x` as long as the inverse exists and is representable.
     *
     * @return `x^{-1}
     * @throws ArithmeticException on non-invertible `x``
     */
    def inv = {
      // double precision of internal computation
      val X = x.setScale(2 * x.precision)
      val i = ONE / X
      if (i.precision < x.precision)
        i.setScale(x.precision - 1)
      else
        i.round(mc)
    }

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
    def stripTrailingZeros = new BigDecimal(x.underlying.stripTrailingZeros(), mc)
  }
}