package scalaam.language.atomlang

import scalaam.core._
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
class AtomlangSemantics[A <: Address, V, T, C, TID <: ThreadIdentifier](addressAllocator: Allocator[A, T, C], tidAllocator: TIDAllocator[TID, T, C])(
    implicit val t: Timestamp[T, C],
    implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends BaseSchemeSemantics[A, V, T, C](addressAllocator)(t, lat)
        with AtomlangPrimitives[A, V, T, C] {
    
    import Action.{Err, NewFuture, Push}
    import schemeLattice._
    
    /**
      * Performs an evaluation step of a given expression.
      *
      * @param e     The expression to evaluate.
      * @param env   The environment in which to evaluate the expression.
      * @param store The store to use for the evaluation.
      * @param t     The current timestamp.
      */
    override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T): Actions = e match {
        case AtomlangDeref(exp, _) => Push(FrameDeref(), exp, env, store)
        case AtomlangFuture(_, _) =>
            val tid = tidAllocator.allocate(e, t)
            val tidv = future(tid)
            NewFuture(tid, tidv, e, env, store) // Let the machine handle the actual thread creation.
        case AtomlangSwap(atomExp, funExp, argExps, _) => Push(FrameSwapAtom(atomExp, funExp, argExps, env), atomExp, env, store)
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
    override def stepKont(v: V, frame: Frame, store: Sto, t: T): Actions = frame match {
        case FrameDeref() => // Todo: extend to atoms.
            val futures = getFutures(v)
            if (futures.isEmpty) {
                Set(Err(TypeError("Cannot dereference non-future values.", v)))
            } else {
                futures.map(tid => Action.DerefFuture(tid, store))
            }
        case FrameSwapAtom(atomExp, funExp, argExps, env) => Push(FrameSwapFun(v, atomExp, funExp, argExps, env), funExp, env, store)
        case FrameSwapFun(atomv, atomExp, funExp, argExps, env) => swapArgs(atomv, atomExp, v, funExp, List(), argExps, env, store, t)
        case FrameSwapArgs(atomv, atomExp, funv, funExp, args, argExp :: rest , env) => swapArgs(atomv, atomExp, funv, funExp, (argExp, v) :: args, rest, env, store, t)
        case _ => super.stepKont(v, frame, store, t)
    }
    
    def swapArgs(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], toEval: List[SchemeExp], env: Env, store: Sto, t: T): Actions = toEval match {
        case Nil => evalSwap(atomv, atomExp, funv, funExp, args, env, store, t)
        case argExp :: _ => Push(FrameSwapArgs(atomv, atomExp, funv, funExp, args, toEval, env), argExp, env, store)
    }
    
    def evalSwap(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], env: Env, store: Sto, t: T): Actions = {
        ???
    }
    
    trait AtomLangFrame extends SchemeFrame
    case class FrameDeref() extends AtomLangFrame
    case class FrameSwapAtom(atomExp: SchemeExp, funExp: SchemeExp, argExps: List[SchemeExp], env: Env) extends AtomLangFrame
    case class FrameSwapFun(atomv: V, atomExp: SchemeExp, funExp: SchemeExp, argExps: List[SchemeExp], env: Env) extends AtomLangFrame
    case class FrameSwapArgs(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], toEval: List[SchemeExp], env: Env) extends SchemeFrame
}