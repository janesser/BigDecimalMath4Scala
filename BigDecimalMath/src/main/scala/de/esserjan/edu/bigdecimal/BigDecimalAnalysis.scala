package de.esserjan.edu.bigdecimal

object BigDecimalAnalysis {
  import de.esserjan.edu.BigDecimalMath._
  import BigDecimalTools._
  
  def exp(x: BigDecimal): BigDecimal = ???

  def ln(x: BigDecimal): BigDecimal = ???
  def lb(x: BigDecimal): BigDecimal = ???
  def log(x: BigDecimal): BigDecimal = ???

  def root(x: BigDecimal, n: Int): BigDecimal = {
    require(n > 1)
    val mc = new java.math.MathContext(x.precision)
    val zero = BigDecimal(0.0, mc)

    if (x == zero) zero
    else if (x < zero) {
      if (n % 2 == 0)
        root(x.negate, n)
      else
        root(x.negate, n).negate
    } else {
      val nth = BigDecimal(n, mc)

      def iter(y: BigDecimal): BigDecimal =
        y - ((y - (x / y.pow(n - 1))) / nth)

      val eps = x.ulp.apply(mc) / nth / x / 2
      def goodEnough(xs: BigDecimal): Boolean =
        (xs * xs - x).abs < eps

      val s = BigDecimal(Math.pow(x.doubleValue, 1.0 / n.toDouble))
      val estimatedRoot =
        try {
          newton(s, iter, goodEnough)
        } catch {
          case NewtonLimitException(x: BigDecimal, iLimit: Int) =>
            println(s"WARNING root($x, $n) hit iteration limit $iLimit; possibly periodic result")
            x
          case NewtonArithmeticException(x: BigDecimal, ex: Throwable) =>
            val msg = ex.getMessage
            println(s"WARNING root($x, $n) hit some arithmetic exception: $msg; possibly division by zero occured")
            x
        }
      import scala.math.BigDecimal.RoundingMode._
      import scala.math.abs
      /* compact by toEngineeringString() and re-parsing with power-of-ten scale */
      BigDecimal(estimatedRoot
        //.setScale(estimatedRoot.precision - x.precision, HALF_UP)
        //.setScale(x.scale, HALF_UP)
        .round(mc)
        .underlying.toEngineeringString())
    }
  }
}