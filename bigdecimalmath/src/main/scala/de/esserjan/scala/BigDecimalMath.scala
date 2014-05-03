package de.esserjan.scala

object BigDecimalMath {
  import bigdecimal.BigDecimalTools._
  import bigdecimal.BigDecimalTrigonometry._
  import bigdecimal.BigDecimalAnalysis._

  implicit class BigIntOps(x: BigInt) {
    val MINUS_ONE = BigInt(-1)
    val ZERO = BigInt(0)
    val ONE = BigInt(1)

    def negate = x * MINUS_ONE

    def toBigDecimal(mc: java.math.MathContext) = BigDecimal(x, mc)

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
    private[this] val ZERO: BigDecimal = new java.math.BigDecimal(0.0, mc)
    private[this] val ONE: BigDecimal = new java.math.BigDecimal(1.0, mc)

    def neg = x * MINUS_ONE

    def invertible: Boolean = x != ZERO && Range(1,1024).isDefinedAt(x.scale)
    def inv =
      if (!invertible)
        throw new ArithmeticException(s"$x is not invertible")
      else x.pow(-1).round(mc)

    def factorial: BigDecimal =
      if (!x.isWhole) throw new ArithmeticException
      else x.toBigInt.factorial.toBigDecimal(mc)

    def sqrt = root(x, 2)
    def cqrt = root(x, 3)

    def sin = sinus(x)
    def cos = cosinus(x)
    def tan = sin / cos
    
    def exp = bigdecimal.BigDecimalAnalysis.exp(x)
    def ln = bigdecimal.BigDecimalAnalysis.ln(x)

    def stripTrailingZeros = BigDecimal(x.underlying.stripTrailingZeros(), mc)
  }
}