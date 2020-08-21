package cyclo

import scalin.immutable.Mat

/* Parses Cyclotomic numbers
*
* The recognized grammar is as follows:
*
*   <expression> ::= <sum> <end>
*   <sum> ::= <product> ( ('+' <product>) | ('-' <product>) )*
*   <product> ::= <factor> ( ('*' <factor>) | ('/' <factor>) )*
*   <factor> ::= <atom> | ('-' <factor>) | ('+' <factor>) | (<atom> '^' ('+' | '-')? <factor>)
*   <atom> ::= (<integer>) | ( 'E' '(' '-'? <integer> ')' ) | ( '(' <sum> ')' )
*   <integer> ::= '0' | ( ['1'-'9'] ['0'-'9']* )
*/
object Parser {

  import fastparse.MultiLineWhitespace._
  import fastparse._

  def mat[_: P]: P[Mat[Cyclo]] = P("[" ~ sum.rep(sep = ",").rep(sep = ";") ~ "]").filter {
    seqSeq => seqSeq.length > 0 && seqSeq.forall(_.length == seqSeq(0).length)
  }.map { seqSeq =>
    val nRows = seqSeq.length
    val nCols = seqSeq(0).length
    scalin.immutable.DenseMat.tabulate(nRows, nCols)((r, c) => seqSeq(r)(c))
  }

  def expression[_: P]: P[Cyclo] = P("" ~ sum ~ End)

  def sum[_: P]: P[Cyclo] = P(product ~ (("+".! | "-".!) ~ product).rep).map {
    case (lhs, rhs) => rhs.foldLeft(lhs) {
      case (acc, ("+", v)) => acc + v
      case (acc, ("-", v)) => acc - v
    }
  }

  def product[_: P]: P[Cyclo] = P(factor ~ (("*".! | "/".!) ~ factor).rep).map {
    case (lhs, rhs) => rhs.foldLeft(lhs) {
      case (acc, ("*", v)) => acc * v
      case (acc, ("/", v)) => acc / v
    }
  }

  def factor[_: P]: P[Cyclo] = P(power | signedFactor | atom)

  def signedFactor[_: P]: P[Cyclo] = P(("+".! | "-".!) ~ factor).map {
    case ("+", v) => v
    case ("-", v) => -v
  }

  def power[_: P]: P[Cyclo] = P(atom ~ "^" ~ (unsignedInt | signedInt)).map {
    case (b, e) => b.pow(e)
  }

  def atom[_: P]: P[Cyclo] = P(integer | rootOfUnity | sqrt | ("(" ~ sum ~ ")"))

  def rootOfUnity[_: P]: P[Cyclo] = P("E" ~ "(" ~ (unsignedInt | signedInt) ~ ")").map(Cyclo.e(_))

  def unsignedInt[_: P]: P[Int] = integerString.map(_.toInt)

  def signedInt[_: P]: P[Int] = P(("+".! ~ unsignedInt) | ("-".! ~ unsignedInt)).map {
    case ("+", e) => e
    case ("-", e) => e
  }

  def integer[_: P]: P[Cyclo] = integerString.map(s => Cyclo(BigInt(s)))

  def integerString[_: P]: P[String] = P("0".!) | P((CharIn("1-9") ~ CharIn("0-9").rep).!)

  def sqrt[_: P]: P[Cyclo] = P("sqrt" ~ "(" ~ sum ~ ")").filter(_.isRational).map(r => Cyclo.sqrt(r.toRational))

}
