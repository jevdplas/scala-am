package scalaam.language.atomlang

import scalaam.core.Position
import scalaam.language.scheme.SchemeExp

/** Dereferencing of a future or of an atom: (deref v) */
case class AtomlangDeref(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString: String = {
    s"(deref $exp)"
  }

  def fv: Set[String] = exp.fv // TODO: verify corectness
}

/** Future creation: (future e) */
case class AtomlangFuture(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString: String = {
    val body = exps.mkString(" ")
    s"(future $body)"
  }

  def fv: Set[String] = exps.flatMap(_.fv).toSet // TODO: verify correctness
}

/** Swapping an atom value: (swap! atom fun args*) */
case class AtomlangSwap(atom: SchemeExp, fun: SchemeExp, args: List[SchemeExp], pos: Position)
    extends SchemeExp {
  override def toString: String = {
    val ags = args.mkString(" ")
    s"(swap! $atom $fun $ags)"
  }

  def fv: Set[String] = atom.fv ++ fun.fv ++ args.flatMap(_.fv).toSet // TODO: verify correctness
}
