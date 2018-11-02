package scalaam.language.atomlang

import scalaam.core.{Address, Allocator, Frame, Timestamp}
import scalaam.language.scheme._

class AtomlangSemantics[A <: Address, V, T, C](allocator: Allocator[A, T, C])(
  implicit val t: Timestamp[T, C],
  implicit val latt: SchemeLattice[V, SchemeExp, A])
  extends BaseSchemeSemantics[A, V, T, C](allocator)(t, latt) {

  case class UnsupportedPrimitive() extends Error

  override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T) = {
    super.stepEval(e, env, store, t)
  }

  override def stepKont(v: V, frame: Frame, store: Sto, t: T) = {
    super.stepKont(v, frame, store, t)
  }
}