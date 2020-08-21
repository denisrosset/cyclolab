package cyclo

import spire.algebra.Sign
import spire.math.Rational
import spire.syntax.signed._
import spire.std.double._
import spire.std.bigInt._
import scala.annotation.tailrec


case class ContinuedFraction(terms: List[BigInt]) {

  def toRational: Rational = if (terms.isEmpty) Rational.zero else {
    def rec(acc: Rational, rest: List[BigInt]): Rational = rest match {
      case h :: t => rec(h + acc.reciprocal, t)
      case Nil => acc
    }
    val t = terms.reverse
    rec(t.head, t.tail)
  }

}

object ContinuedFraction {

  implicit class RationalIntegerPart(val a: Rational) extends AnyVal {
    def integerPart: Rational = a.sign match {
      case Sign.Zero => a
      case Sign.Positive => a.floor
      case Sign.Negative => a.ceil
    }
  }

  implicit class DoubleIntegerPart(val a: Double) extends AnyVal {
    def integerPart: Double = a.sign match {
      case Sign.Zero => a
      case Sign.Positive => a.floor
      case Sign.Negative => a.ceil
    }
  }

  def apply(bi: BigInt): ContinuedFraction = ContinuedFraction(bi :: Nil)

  def apply(r: Rational): ContinuedFraction =
    if (r.isZero) ContinuedFraction(Nil) else {
    @tailrec def rec(rem: Rational, acc: List[BigInt]): List[BigInt] = {
      val integerPart = rem.integerPart
      val fractionalPart = rem - integerPart
      val acc1 = integerPart.toBigInt :: acc
      if (fractionalPart.isZero) acc1.reverse else rec(fractionalPart.reciprocal, acc1)
    }
    ContinuedFraction(rec(r, Nil))
  }

  def bestApproximation(l: Double, r: Double): Rational = {
    val lc = apply(Rational(l))
    val rc = apply(Rational(r))
    @tailrec def rec(l1: List[BigInt], r1: List[BigInt], acc: List[BigInt]): List[BigInt] = (l1, r1) match {
      case (lh :: lt, rh :: rt) if lh == rh => rec(lt, rt, lh :: acc)
      case (lh :: lt, rh :: rt) => (lh.min(rh) + 1) :: acc
      case (lh :: lt, Nil) => (lh + 1) :: acc
      case (Nil, rh :: rt) => (rh + 1) :: acc
      case (Nil, Nil) => acc
    }
    ContinuedFraction(rec(lc.terms, rc.terms, Nil).reverse).toRational
  }

}
