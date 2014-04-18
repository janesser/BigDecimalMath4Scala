package de.esserjan.edu.bigdecimal

object BigDecimalTrigonometry {
  import de.esserjan.edu.BigDecimalMath._
  import BigDecimalTools._
  
  /**
   * Euler's constant Pi.
   * [[The University of Arizona - Computer Science - PI|http://www.cs.arizona.edu/icon/oddsends/pi.htm]]
   *
   * Precision is actually PI_PRECISION = 1638.
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

  /**
   * @param mc MathContext indicating the requested precision (and rounding-mode)
   * @return PI rounded to the requested precision
   */
  def pi(mc: java.math.MathContext): BigDecimal = {
    if (mc.getPrecision() < PI_PRECISION) PI.round(mc)
    else if (mc.getPrecision() > PI_PRECISION) throw new NotImplementedError
    else PI
  }
  /**
   * @param precision plain-precision as integer
   * @return (see above)
   */
  def pi(precision: Int): BigDecimal = pi(new java.math.MathContext(precision))

  /**
   * @param x in radial measure (a period lasts from 0 to 2 \pi) (precision-driver)
   * @return sinus at point {{x}}
   */
  def sinus(x: BigDecimal): BigDecimal = {
    val mc = new java.math.MathContext(x.precision)

    val Pi = pi(mc)
    val twoPi = Pi * 2
    val halfPi = Pi / 2

    val minusOne: BigDecimal = BigDecimal(-1.0, mc).setScale(x.scale)
    val zero: BigDecimal = BigDecimal(0.0, mc)
    val one: BigDecimal = BigDecimal(1.0, mc).setScale(x.scale)

    if (x == Pi) zero
    else if (x == halfPi) one
    else if (x == halfPi * 3) minusOne
    else if (x.signum < 0) sinus(x.neg).neg
    else if (x > twoPi) sinus(x - twoPi)
    else if (x > Pi) sinus(x - Pi).neg
    else {
      /*
       * x^(2k+1) < x.ulp; (2k+1)*log10(x) < -x.precision; 2k*log10(x)< -x.precision;
       */
      val n: Int = (x.precision / java.lang.Math.log10(x.doubleValue)).toInt / 2
      evalSeries(x,
        (k: Int) =>
          (x: BigDecimal) =>
            BigDecimal(-1).pow(k) * x.pow(2 * k + 1) / BigInt(2 * k + 1).factorial.toBigDecimal(mc),
        n)
    }
  }

  def cosinus(x: BigDecimal): BigDecimal = {
    val mc = new java.math.MathContext(x.precision)

    val Pi = pi(mc)
    val twoPi = Pi * 2
    val halfPi = Pi / 2

    val minusOne: BigDecimal = BigDecimal(-1.0, mc).setScale(x.scale)
    val zero: BigDecimal = BigDecimal(0.0, mc)
    val one: BigDecimal = BigDecimal(1.0, mc).setScale(x.scale)

    if (x == twoPi) one
    else if (x == Pi) minusOne
    else if (x == halfPi) zero
    else if (x.signum < 0) cosinus(x.neg)
    else if (x > twoPi) cosinus(x - twoPi)
    else if (x > Pi) cosinus(twoPi - x)
    else sinus(x + (pi(x.precision) / 2))
  }
}