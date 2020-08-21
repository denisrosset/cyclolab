package cyclo

import cats.kernel.Semigroup
import scalin.Pivot
import scalin.immutable.DenseMat
import spire.algebra.Eq
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.util.Opt

import scala.collection.mutable

object Lab {

  def isWhole(arg: Array[Cyclo]): Array[Boolean] = arg.map(c => c.isRational && c.toRational.isWhole)

  def isRational(arg: Array[Cyclo]): Array[Boolean] = arg.map(_.isRational)
  
  def E(orders: Array[Int]): Array[Cyclo] = orders.map(Cyclo.e)

  def parse(strings: Array[String]): Array[Cyclo] = strings.map(s => fastparse.parse(s, Parser.expression(_)).fold((_, _, _) => null, (c, _) => c))

  def cycloToDouble(c: Cyclo): (Double, Double) = {
    val r = realCycloToDouble(RealCyclo.real(c))
    val i = realCycloToDouble(RealCyclo.imag(c))
    (r, i)
  }

  /** Converts an array of cyclotomic numbers to complex pairs of double floating-point numbers
    *
    * @param cyclos Array of cyclotomic numbers
    * @return A pair of arrays representing the real and imaginary parts
    */
  def toDouble(cyclos: Array[Cyclo]): Array[Array[Double]] = {
    val n = cyclos.length
    val real = new Array[Double](n)
    val imag = new Array[Double](n)
    val values = mutable.HashMap.empty[Cyclo, (Double, Double)]
    cforRange(0 until n) { ind =>
      val c = cyclos(ind)
      if (c.isRational) {
        real(ind) = c.toRational.toDouble
      } else {
        val (r, i) = values.getOrElseUpdate(c, cycloToDouble(c))
        real(ind) = r
        imag(ind) = i
      }
    }
    Array(real, imag)
  }

  def plus(lhs: Array[Cyclo], rhs: Array[Cyclo]): Array[Cyclo] = {
    val n = lhs.length
    assert(rhs.length == n)
    Array.tabulate(n)(i => lhs(i) + rhs(i))
  }

  def minus(lhs: Array[Cyclo], rhs: Array[Cyclo]): Array[Cyclo] = {
    val n = lhs.length
    assert(rhs.length == n)
    Array.tabulate(n)(i => lhs(i) - rhs(i))
  }

  def pw_times(lhs: Array[Cyclo], rhs: Array[Cyclo]): Array[Cyclo] = {
    val n = lhs.length
    assert(rhs.length == n)
    Array.tabulate(n)(i => lhs(i) * rhs(i))
  }

  def pw_divide(lhs: Array[Cyclo], rhs: Array[Cyclo]): Array[Cyclo] = {
    val n = lhs.length
    assert(rhs.length == n)
    Array.tabulate(n)(i => lhs(i) / rhs(i))
  }

  def negate(cyclos: Array[Cyclo]): Array[Cyclo] = cyclos.map(- _)

  def eqv(lhs: Array[Cyclo], rhs: Array[Cyclo]): Array[Boolean] = (lhs zip rhs).map { case (l, r) => l === r }

  def times(l: Int, m: Int, n: Int, lhs0: Array[Cyclo], rhs0: Array[Cyclo]): Array[Cyclo] = {
    import scalin.immutable.dense._
    val lhs = new scalin.immutable.DenseMat[Cyclo](l, m, lhs0.asInstanceOf[Array[AnyRef]])
    val rhs = new scalin.immutable.DenseMat[Cyclo](m, n, rhs0.asInstanceOf[Array[AnyRef]])
    val res = lhs * rhs
    res.data.map(_.asInstanceOf[Cyclo])
  }

  def plusScalar(lhs: Array[Cyclo], rhs: Cyclo): Array[Cyclo] = lhs.map(_ + rhs)

  def minusScalar(lhs: Array[Cyclo], rhs: Cyclo): Array[Cyclo] = lhs.map(_ - rhs)

  def timesScalar(lhs: Array[Cyclo], rhs: Cyclo): Array[Cyclo] = lhs.map(_ * rhs)

  def divideScalar(lhs: Array[Cyclo], rhs: Cyclo): Array[Cyclo] = lhs.map(_ / rhs)

  def conjugate(cyclos: Array[Cyclo]): Array[Cyclo] = cyclos.map(_.conjugate)

  def realCycloToDouble(c: RealCyclo): Double = if (c.isZero) 0.0 else if (c.isRational) c.toRational.toDouble else c.toAlgebraic.toDouble

  def fromInt(i: Int): Cyclo = Cyclo(i)

  def approximate(lb: Array[Double], ub: Array[Double]): Array[Cyclo] = (lb zip ub) map {
    case (l, u) => Cyclo(ContinuedFraction.bestApproximation(l, u))
  }
  
  def fromDouble(d: Array[Double]): Array[Cyclo] = d.map(x => Cyclo(Rational(x)))

  def fromRational(numerators: Array[Long], denominators: Array[Long]): Array[Cyclo] =
    (numerators zip denominators).map { case (n, d) => Cyclo(Rational(n, d)) }

  def sqrt(i: Int): Cyclo = sqrt(i, 1)

  def sqrt(num: Int, den: Int): Cyclo = Cyclo.sqrt(Rational(num, den))

  def pw_power(lhs: Array[Cyclo], exp: Int): Array[Cyclo] = lhs.map(_.pow(exp))

  def power(n: Int, c: Array[Cyclo], exp: Int): Array[Cyclo] =
    if (exp == 0) Array.tabulate(n, n)( (i, j) => if (i == j) Cyclo.one else Cyclo.zero ).flatten
    else if (exp < 0) power(n, inverse(n, c), -exp)
    else if (exp == 1) c
    else {
      val lhs = new DenseMat[Cyclo](n, n, c.asInstanceOf[Array[AnyRef]])
      import scalin.immutable.dense._
      implicit object compose extends Semigroup[DenseMat[Cyclo]] {
        override def combine(x: DenseMat[Cyclo], y: DenseMat[Cyclo]): DenseMat[Cyclo] = x * y
      }
      compose.combineN(lhs, exp).data.map(_.asInstanceOf[Cyclo])
    }

  def inverse(n: Int, c: Array[Cyclo]): Array[Cyclo] = {
    implicit object cycloPivot extends Pivot[Cyclo] {
      def priority(a: Cyclo): Double = if (a.isZero) 0 else (0 until a.nTerms).map(i => Pivot[Rational].priority(a.coefficient(i))).reduce(_ + _)
      def optionalExactEq: Opt[Eq[Cyclo]] = Opt(Eq[Cyclo])
      def closeToZero(a: Cyclo): Boolean = a.isZero
    }
    import scalin.computation.Inverse.denseInverse
    import scalin.immutable.dense._
    val lhs = new DenseMat[Cyclo](n, n, c.asInstanceOf[Array[AnyRef]])
    lhs.inverse.data.map(_.asInstanceOf[Cyclo])
  }

  def printE(order: Int, exp: Int, nonEmptyPrefix: String, one: String): String =
    if (exp == 0) one
    else nonEmptyPrefix + (if (exp == 1) s"E($order)" else s"E($order)^$exp")

  def printTerm(c: Rational, order: Int, exp: Int, opPrefix: Boolean): String =
    if (c < 0 && opPrefix) " - " + printTerm(-c, order, exp, false)
    else if (c < 0 && !opPrefix) "-" + printTerm(-c, order, exp, false)
    else if (c > 0 && opPrefix) " + " + printTerm(c, order, exp, false)
    else if (c == 1) printE(order, exp, "", "1")
    else if (c.denominator.isOne) c.numerator.toString + printE(order, exp, "*", "")
    else if (c.numerator.isOne) printE(order, exp, "", "1") + "/" + c.denominator.toString
    else c.numerator.toString + printE(order, exp, "*", "") + "/" + c.denominator.toString

  def printCyclo(c: Cyclo): String =
    if (c.isZero) "0"
    else printTerm(c.coefficient(0), c.order, c.exponent(0), false) + (1 until c.nTerms).map(i => printTerm(c.coefficient(i), c.order, c.exponent(i), true)).mkString

  def print(c: Array[Cyclo]): Array[String] = c.map(printCyclo)

  def sqrt(c: Array[Cyclo]): Array[Cyclo] = {
    assert(c.forall(_.isRational), "Can only take the square root of rational numbers")
    c.map(r => Cyclo.sqrt(r.toRational))
  }

}
