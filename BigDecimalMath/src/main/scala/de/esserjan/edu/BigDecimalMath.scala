package de.esserjan.edu

object BigDecimalMath {

  val MINUS_ONE: BigDecimal = -1
  val ZERO: BigDecimal = 0
  val ONE: BigDecimal = 1

  /**
   * Euler's constant Pi.
   * http://www.cs.arizona.edu/icon/oddsends/pi.htm
   */
  private[this] val PI: BigDecimal = new java.math.BigDecimal("""3.14159265358979323846264338327950288419716939937510582097494459230781640628620
899862803482534211706798214808651328230664709384460955058223172535940812848111
745028410270193852110555964462294895493038196442881097566593344612847564823378
678316527120190914564856692346034861045432664821339360726024914127372458700660
631558817488152092096282925409171536436789259036001133053054882046652138414695
194151160943305727036575959195309218611738193261179310511854807446237996274956
735188575272489122793818301194912983367336244065664308602139494639522473719070
217986094370277053921717629317675238467481846766940513200056812714526356082778
577134275778960917363717872146844090122495343014654958537105079227968925892354
201995611212902196086403441815981362977477130996051870721134999999837297804995
105973173281609631859502445945534690830264252230825334468503526193118817101000
313783875288658753320838142061717766914730359825349042875546873115956286388235
378759375195778185778053217122680661300192787661119590921642019893809525720106
548586327886593615338182796823030195203530185296899577362259941389124972177528
347913151557485724245415069595082953311686172785588907509838175463746493931925
506040092770167113900984882401285836160356370766010471018194295559619894676783
744944825537977472684710404753464620804668425906949129331367702898915210475216
205696602405803815019351125338243003558764024749647326391419927260426992279678
235478163600934172164121992458631503028618297455570674983850549458858692699569
092721079750930295532116534498720275596023648066549911988183479775356636980742
654252786255181841757467289097777279380008164706001614524919217321721477235014""".replaceAllLiterally("\n", ""))
  val PI_PRECISION = PI.precision

  def pi(mc: java.math.MathContext): BigDecimal = {
    if (mc.getPrecision() < PI_PRECISION) PI.round(mc)
    else if (mc.getPrecision() > PI_PRECISION) throw new NotImplementedError
    else PI
  }
  def pi(precision: Int): BigDecimal = pi(new java.math.MathContext(precision))

  def sqrt(x: BigDecimal): BigDecimal =
    com.github.oxlade39.scalabetfair.math.BigDecimalMath.sqrt(x)

  implicit class BigIntOps(x: BigInt) {
    val MINUS_ONE = BigInt(-1)
    val ZERO = BigInt(0)
    val ONE = BigInt(1)

    def negate = x * MINUS_ONE

    def toBigDecimal = BigDecimal(x)

    def faculty: BigInt = {
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

    def negate = x * MINUS_ONE

    def faculty: BigDecimal =
      if (!x.isWhole) throw new IllegalArgumentException
      else x.toBigInt.faculty.toBigDecimal

    def sin = sinus(x)
    def cos = cosinus(x)
    def tan = sin / cos
  }

  def sumSerie(x: BigDecimal,
    fx: (Int) => (BigDecimal => BigDecimal),
    n: Int, k: Int = 0, acc: BigDecimal = 0): BigDecimal = {
    if (k < n) sumSerie(x, fx, n, k + 1, acc + fx(k)(x))
    else acc.round(new java.math.MathContext(x.precision))
  }

  def sinus(x: BigDecimal): BigDecimal = {
    val Pi = pi(new java.math.MathContext(x.precision))
    val TwoPi = Pi * 2
    val HalfPi = Pi / 2

    if (x == ZERO || x == Pi) ZERO
    else if (x == HalfPi) ONE
    else if (x == HalfPi * 3) MINUS_ONE
    else if (x < ZERO) sinus(x.negate).negate
    else if (x > TwoPi) sinus(x % TwoPi)
    else if (x > Pi) sinus(x - Pi).negate
    else if (x > HalfPi) sinus(x - HalfPi)
    else {
      /*
       * x^(2k+1) < x.ulp; (2k+1)*log10(x) < -x.precision; 2k*log10(x)< -x.precision;
       */
      val n: Int = (x.precision / java.lang.Math.log10(x.doubleValue)).toInt / 2
      sumSerie(x,
        (k: Int) =>
          (x: BigDecimal) =>
            BigDecimal(-1).pow(k) * x.pow(2 * k + 1) / BigDecimal(2 * k + 1).faculty,
        n)
    }
  }

  def cosinus(x: BigDecimal): BigDecimal =
    sinus(pi(x.precision) / 4 + x)
}