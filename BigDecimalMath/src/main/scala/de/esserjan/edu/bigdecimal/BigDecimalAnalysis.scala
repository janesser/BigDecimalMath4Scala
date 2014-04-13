package de.esserjan.edu.bigdecimal

object BigDecimalAnalysis {
  import de.esserjan.edu.BigDecimalMath._
  import BigDecimalTools._

  private[this] val E: BigDecimal = new java.math.BigDecimal("""2.71828182845904523536028747135266249775724709369995957496696762772407663035354
759457138217852516642742746639193200305992181741359662904357290033429526059563
073813232862794349076323382988075319525101901157383418793070215408914993488416
750924476146066808226480016847741185374234544243710753907774499206955170276183
860626133138458300075204493382656029760673711320070932870912744374704723069697
720931014169283681902551510865746377211125238978442505695369677078544996996794
686445490598793163688923009879312773617821542499922957635148220826989519366803
318252886939849646510582093923982948879332036250944311730123819706841614039701
983767932068328237646480429531180232878250981945581530175671736133206981125099
618188159304169035159888851934580727386673858942287922849989208680582574927961
048419844436346324496848756023362482704197862320900216099023530436994184914631
409343173814364054625315209618369088870701676839642437814059271456354906130310
720851038375051011574770417189861068739696552126715468895703503540212340784981
933432106817012100562788023519303322474501585390473041995777709350366041699732
972508868769664035557071622684471625607988265178713419512466520103059212366771
943252786753985589448969709640975459185695638023637016211204774272283648961342
251644507818244235294863637214174023889344124796357437026375529444833799801612
549227850925778256209262264832627793338656648162772516401910590049164499828931""".replaceAllLiterally("\n", ""))

  val E_PRECISION = E.precision

  def e(mc: java.math.MathContext): BigDecimal =
    if (mc.getPrecision < E_PRECISION) E.round(mc)
    else if (mc.getPrecision > E_PRECISION)
      exp(BigDecimal(1.0, mc).setScale(mc.getPrecision))
    else E

  def e(precision: Int): BigDecimal =
    e(new java.math.MathContext(precision))

  def exp(x: BigDecimal): BigDecimal = {
    val mc = new java.math.MathContext(x.precision)
    val zero = BigDecimal(0.0, mc)
    lazy val one = BigDecimal(1.0, mc)
    if (x < zero)
      one / exp(x.negate)
    else if (x == zero)
      one
    else {
      val n = 32
      val errorRatio =
        Range(0, 2).map(n - _).reduce((x, y) => x * y)
      if (x.pow(n) < errorRatio * x.ulp) {
        def fn =
          (n: Int) =>
            (x: BigDecimal) =>
              x.pow(n) / BigDecimal(n, mc).factorial
        // extra precision
        evalSeries(x, fn, n).round(mc)
      } else
        // pull x to 1.0
        exp(x / 10).pow(10)
    }
  }

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