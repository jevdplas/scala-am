package scalaam.language.atomlang

import scalaam.core.{Address, Allocator, Frame, Timestamp}
import scalaam.language.scheme._

/**
  * This class extends SchemeSemantics with constructs for concurrency using futures and atoms.
  * @param allocator    An allocator for memory.
  * @param t            A timestamp.
  * @param latt         A lattice.
  * @tparam A   The type for addresses.
  * @tparam V   The type for values.
  * @tparam T   The type for tiemstamps.
  * @tparam C
  */
class AtomlangSemantics[A <: Address, V, T, C](allocator: Allocator[A, T, C])(
    implicit val t: Timestamp[T, C],
    implicit val latt: SchemeLattice[V, SchemeExp, A])
    extends BaseSchemeSemantics[A, V, T, C](allocator)(t, latt) {
    
    override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T) = e match {
        case AtomlangAtom(_, _) => Action.Err(NotSupported("AT"))
        case AtomlangDeref(_, _) => Action.Err(NotSupported("AT"))
        case AtomlangFuture(_, _) => Action.NewFuture(e, env, store) // Let the machine handle the actual thread creation.
        case _ => super.stepEval(e, env, store, t)
    }
    
    override def stepKont(v: V, frame: Frame, store: Sto, t: T) =  frame match {
        case _ => super.stepKont(v, frame, store, t)
    }
}