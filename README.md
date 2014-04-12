# BigDecimal Arithmetics
Be as precise as you like (or as much as you can afford in terms of runtime).
BigDecimal precision is maintained throughout calculations. That means function parameter and result have equal scale. ZERO is a special case anyway.

Actually scala.math.BigDecimal and alike are almost equal (or worse) than their Java correspondants. 
Having some functional structures at hand may improve the way to think about computating formulas strongly.

Intention of this extending library is:

* to make use of BigDecimal easier
* to make code more fluent to read
* to give control over the scale of calculation

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
*Negation* by multiplication with {{BigInt(-1)}}. As long as the number was not zero before, the result's signum multiplied with the input signum is always "-1".

*Factorial* with tail-recursive ideomatic implementation. A negative signum will not affect the calculation but result in a negative result.

## BigDecimalOps
*Negation* by multiplication with {{BigDecimal(-1)}}.
Last mentioned inherits MathContext from input.
As long as the number was not zero before, the result's signum multiplied with the input signum is always "-1".

*Factorial* based on BigInt#factorial. If not BigDecimal#isWhole this operation has no result, e.g. thows an IllegalArgumentException.

### Trigonometry
Sine (lat. *Sinus*), except for trivial cases, is interpolated by a taylor expansion. The number of series element summed fits the precision requested.

Cosine (lat. *Cosinus*), except for trivial cases, is based on the sinus-function.

### Roots
*Square-root* evaluated by newton-iteration.

Actually unstable for _x_ very close to zero and very big numbers.

## Misc.

- *evalSeries* Evaluate series summation for a limited amount of components.

		def evalSeries(x: BigDecimal,
			fx: (Int) => (BigDecimal => BigDecimal),
			n: Int)
    
- *newton* Approximating iteration with step limit and specific *goodEnough*.

		def newton(
			x: BigDecimal,
			fn: BigDecimal => BigDecimal,
			goodEnough: BigDecimal => Boolean)

# Contribution
Feel free to offer pull requests or create issues for your suggestions.

# Contact
[januszesser@twitter](https://twitter.com/januszesser)

[![Flattr this](//api.flattr.com/button/flattr-badge-large.png "Flattr this")](https://flattr.com/submit/auto?user_id=januszesser&url=http%3A%2F%2Fbitbucket.org%2Fjanesser%2Fbigdecimalmath)

# See also
Based on [A Java Math.BigDecimal Implementation of Core Mathematical Functions](http://arxiv.org/abs/0908.3030).

Inspired by [BigDecimalMath.scala](https://gist.github.com/oxlade39/5752033).
