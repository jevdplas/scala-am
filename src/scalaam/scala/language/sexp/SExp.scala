package scalaam.language.sexp

import scalaam.core.{Position, Identifier, Expression}

/**
  * S-expressions and related values
  */
sealed abstract class Value
case class ValueString(value: String) extends Value {
  override def toString = s""""$value""""
}
case class ValueSymbol(sym: String) extends Value {
  override def toString = sym
}
case class ValueInteger(value: Int) extends Value {
  override def toString = value.toString
}
case class ValueReal(value: Double) extends Value {
  override def toString =
    f"$value%e" // Might not preserve full precision, but will be in a Scheme-compatible format
}
case class ValueBoolean(value: Boolean) extends Value {
  override def toString =
    if (value) {
      "#t"
    } else {
      "#f"
    }
}
case class ValueCharacter(value: Char) extends Value {
  override def toString = s"#\\$value"
}
object ValueNil extends Value {
  override def toString = "()"
}

/**
  * Abstract grammar elements for S-expressions include some positional
  * information. This serves two purposes: identify where the s-expression
  * resides in the input file, and as tagging information for the abstract
  * machine.
  */
/* [merge] removed this */
// trait SExp extends Expression
/* [merge] instead of this */
trait SExp extends Expression {
  val pos: Position
  def fv = Set()
}
/* [merge] end */

/**
  * An s-expression is made of pairs, e.g., (foo bar) is represented as the pair
  * with identifier foo as car and another pair -- with identifier bar as car and
  * value nil as cdr -- as cdr. Pairs are pretty-printed when converted to
  * string. i.e., (foo bar) is stringified as (foo bar) and not (foo . (bar
  * . ()))
  */
case class SExpPair(car: SExp, cdr: SExp, pos: Position) extends SExp {
  override def toString = {
    val content = toStringRest
    s"($content)"
  }
  def toStringRest: String =
    cdr match {
      case pair: SExpPair =>
        val rest = pair.toStringRest
        s"$car $rest"
      case SExpValue(ValueNil, _) => s"$car"
      case _                      => s"$car . $cdr"
    }
}

object SExpList {
  /* Alternative constructor to automatically construct a bunch of pair from a
   * list of expressions */
  def apply(content: List[SExp], pos: Position) = fromList(content, pos)

  def fromList(content: List[SExp], pos: Position): SExp = content match {
    case Nil          => SExpValue(ValueNil, pos)
    case head :: tail => SExpPair(head, SExpList(tail, pos), head.pos)
  }
}

/**
  * An identifier, such as foo, bar, etc.
  */
case class SExpId(id: Identifier) extends SExp {
  val pos               = id.pos
  override def toString = id.toString
}

/**
  * A literal value, such as 1, "foo", 'foo, etc.
  */
case class SExpValue(value: Value, pos: Position) extends SExp {
  override def toString = value.toString
}

/**
  * A quoted element, such as 'foo, '(foo (bar)), etc.
  */
case class SExpQuoted(content: SExp, pos: Position) extends SExp {
  override def toString = s"'$content"
}
