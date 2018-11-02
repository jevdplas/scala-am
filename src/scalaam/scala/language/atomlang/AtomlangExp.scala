package scalaam.language.atomlang

import scalaam.core.Position
import scalaam.language.scheme.SchemeExp

case class AtomlangDeref(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString: String = {
    s"(deref $exp)"
  }
}

case class AtomlangFuture(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val body = exps.mkString(" ")
    s"(future $body)"
  }
}
