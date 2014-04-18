# BigDecimal Arithmetics for Scala
Be as precise as you like (or as much as you can afford in terms of runtime).
BigDecimal precision is maintained throughout calculations. That means function parameter and result have equal scale. ZERO is a special case anyway.

Actually scala.math.BigDecimal and alike are almost equal (or worse) than their Java correspondants. 
Having some functional structures at hand may improve the way to think about computating formulas.

Intention of this extending library is:

* to make use of BigDecimal easier
* to make code more fluent to read
* to give control over the scale and precision of calculation

Steps to integrate:

- get and compile source-code
		
		git clone https://bitbucket.org/janesser/bigdecimalmath.git
		cd BigDecimalMath
		mvn install
		
- insert into to your pom.xml (or alike)

		<dependency>
			<groupId>de.esserjan.edu</groupId>
			<artifactId>BigDecimalMath</artifactId>
			<version>0.0.1-SNAPSHOT</version>
		</dependency>

- insert into your code

		import de.esserjan.edu.BigDecimalMath._
	
## BigIntOps 
*Negation* by multiplication with `BigInt(-1)`. As long as the number was not zero before, the result's signum multiplied with the input signum is always "-1".

*Factorial* with tail-recursive ideomatic implementation. A negative signum will not affect the calculation but result in a negative result.

## BigDecimalOps
*Negation* by multiplication with `BigDecimal(-1)`.
Last mentioned inherits MathContext from input.
As long as the number was not zero before, the result's signum multiplied with the input signum is always "-1".

*Factorial* based on BigInt#factorial. If not BigDecimal#isWhole this operation has no result, e.g. thows an IllegalArgumentException.
TODO Guess precision required for result, because it is a magnitude more than the input.

### Trigonometry
Sine (lat. *Sinus*), except for trivial cases, is interpolated by a taylor expansion.

Cosine (lat. *Cosinus*), except for trivial cases, is based on the sinus-function.

The approximation is dynamically continued until the precision reaches the input's ULP.

### Roots
*Square-root* evaluated by newton-iteration.

Compaction of BigDecimal is done with reparsing `toEngineeringString()`, so that the result is deflated to some power-of-ten term.

The approximation is dynamically continued until the precision reaches the input's ULP.

### Exp
*Exp*, except for trivial cases, is interpolated by a taylor expansion.

The approximation is dynamically continued until the precision reaches the input's ULP.

TODO logarithmics
## Misc.
### BigDecimalTools
Some generic computation algorithms.

- *evalSeries* Evaluate series summation.
The goodEnough criterium function is going to determine the number of components summed dynamically,
given the MathContext, the number of already summed components, the current component's and the accumulator's value.
Some generic goodEnough criteria are predefined.
		
		trait EvalSeriesGoodEnough extends Function4[java.math.MathContext, Int, BigDecimal, BigDecimal, Boolean]
		case class SinceIndexGoodEnough(val n: Int) extends EvalSeriesGoodEnough {
			def apply(mc: java.math.MathContext, k: Int, fkx: BigDecimal, acc: BigDecimal): Boolean = k > n
		}

		def evalSeries(x: BigDecimal,
			f: (Int) => (BigDecimal => BigDecimal),
			goodEnough: EvalSeriesGoodEnough):BigDecimal
    
- *newton* Approximate by iteration.
The goodEnough criterium function implies the determination of the iteration,
given the number of the iteration step and the current approximation.
Some generic goodEnough criteria are predefined.

		trait NewtonGoodEnough extends Function2[Int, BigDecimal, Boolean]
		case class NewtonResultPrecise(fn: BigDecimal => BigDecimal, precision: BigDecimal) extends NewtonGoodEnough {    
			def apply(i: Int, x: BigDecimal): Boolean = {
				val mc = new java.math.MathContext(x.precision)
				val delta = x - fn(x)
				delta.abs <= precision
			}
		}

		def newton(
			x: BigDecimal,
			fn: BigDecimal => BigDecimal,
			goodEnough: NewtonGoodEnough): BigDecimal

# Licence
TODO Apache Licence?

# Releases
0.0.1 *work in progress* Q2'14 with some basic set of operations.

# Contribution
Feel free to offer pull requests or create issues for your suggestions.

# Contact
[januszesser@twitter](https://twitter.com/januszesser)

[![Flattr this](//api.flattr.com/button/flattr-badge-large.png "Flattr this")](https://flattr.com/submit/auto?user_id=januszesser&url=http%3A%2F%2Fbitbucket.org%2Fjanesser%2Fbigdecimalmath)

# See also
Based on [A Java Math.BigDecimal Implementation of Core Mathematical Functions](http://arxiv.org/abs/0908.3030).

Inspired by [BigDecimalMath.scala](https://gist.github.com/oxlade39/5752033).
