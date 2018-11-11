package scalaam.language.atomlang

import scalaam.core.{Address, Allocator, Frame, Timestamp}
import scalaam.language.scheme._

/**
  * This class extends SchemeSemantics with constructs for concurrency using futures and atoms.
  *
  * @param allocator An allocator for memory.
  * @param t         A timestamp.
  * @param latt      A lattice.
  * @tparam A The type of addresses.
  * @tparam V The type of values.
  * @tparam T The type of timestamps.
  * @tparam C The type of expressions.
  */
class AtomlangSemantics[A <: Address, V, T, C](allocator: Allocator[A, T, C])(
    implicit val t: Timestamp[T, C],
    implicit val latt: SchemeLattice[V, SchemeExp, A])
    extends BaseSchemeSemantics[A, V, T, C](allocator)(t, latt)
    with AtomlangPrimitives[A, V, T, C] {
    
    /**
      * Performs an evaluation step of a given expression.
      *
      * @param e     The expression to evaluate.
      * @param env   The environment in which to evaluate the expression.
      * @param store The store to use for the evaluation.
      * @param t     The current timestamp.
      */
    override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T) = e match {
        // case AtomlangAtom(_, _) => Action.Err(NotSupported("AT")) => Implemented as a regular primitive.
        case AtomlangDeref(_, _) => Action.Err(NotSupported("AT"))
        case AtomlangFuture(_, _) => Action.NewFuture(e, env, store) // Let the machine handle the actual thread creation.
        case _ => super.stepEval(e, env, store, t)
    }
    
    /**
      * Performs a continuation step after the evaluator reached a value.
      *
      * @param v     The value that was reached by evaluation.
      * @param frame The topmost stackframe/continuation frame.
      * @param store The store to continue with.
      * @param t     The current timestamp.
      */
    override def stepKont(v: V, frame: Frame, store: Sto, t: T) = frame match {
        case _ => super.stepKont(v, frame, store, t)
    }
}