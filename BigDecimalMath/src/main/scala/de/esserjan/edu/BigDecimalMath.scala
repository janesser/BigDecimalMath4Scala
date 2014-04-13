package de.esserjan.edu

object BigDecimalMath {
  import bigdecimal.BigDecimalTools._
  import bigdecimal.BigDecimalTrigonometry._
  import bigdecimal.BigDecimalAnalysis._

  implicit class BigIntOps(x: BigInt) {
    val MINUS_ONE = BigInt(-1)
    val ZERO = BigInt(0)
    val ONE = BigInt(1)

    def negate = x * MINUS_ONE

    def toBigDecimal = BigDecimal(x)

    def factorial: BigInt = {
      @scala.annotation.tailrec
      def fac(n: BigInt, acc: BigInt = ONE): BigInt = {
        if (n == ZERO || n == ONE) acc
        else if (n < ZERO) fac(n.negate, acc.negate)
        else fac(n - 1, acc * n)
      }
      fac(x)
    }
  }

  implicit class BigDecimalOps(x: BigDecimal) {
    private[this] val mc = new java.math.MathContext(x.precision)
    private[this] val MINUS_ONE: BigDecimal = new java.math.BigDecimal(-1.0, mc)

    def negate = x * MINUS_ONE

    def factorial: BigDecimal =
      if (!x.isWhole) throw new ArithmeticException
      else BigDecimal(x.toBigInt.factorial, mc)

    def sqrt = root(x, 2)
    def cqrt = root(x, 3)

    def sin = sinus(x)
    def cos = cosinus(x)
    def tan = sin / cos

    def stripTrailingZeros = BigDecimal(x.underlying.stripTrailingZeros(), mc)
  }
}