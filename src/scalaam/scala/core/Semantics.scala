package scalaam.core

import scalaam.core.Effects.Effects

trait Frame extends SmartHash

trait Semantics[Exp, Addr <: Address, V, T, C] {
    implicit val timestamp: Timestamp[T, C]
    implicit val lattice: Lattice[V]
    val allocator: Allocator[Addr, T, C]
    
    object Action {
        trait A
        case class  Value(v: V,                                                                     store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        case class   Push(frame: Frame,                             e: Exp, env: Environment[Addr], store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        case class   Eval(e: Exp,                                           env: Environment[Addr], store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        case class StepIn(fexp: Exp, clo: (Exp, Environment[Addr]), e: Exp, env: Environment[Addr], store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        case class Err(err: Error) extends A
        
        // Action used when a new future needs to be created.
        case class NewFuture[TID <: ThreadIdentifier, F](tid: TID, tidv: V, first: Exp, fram: F, env: Environment[Addr], store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        case class  DerefFuture[TID <: ThreadIdentifier](tid: TID,                                                       store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        // case class         Swap[E](value: V, atomExp: E, funv: V, funExp: E, args: List[(E, V)], env: Environment[Addr], store: Store[Addr, V], effs: Effects[Addr] = Set.empty) extends A
        
        val None: Set[A] = Set.empty
        implicit def actionToSet(act: A): Set[A] = Set(act)
        implicit def fromMF(mf: MayFail[A, Error]): Set[A] = mf match {
            case MayFailSuccess(a) => Set(a)
            case MayFailError(errs) => errs.map(e => Err(e))
            case MayFailBoth(a, errs) => errs.map(e => Err(e)) ++ Set(a)
        }
    }
    
    def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, V], t: T): Set[Action.A]
    def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: T): Set[Action.A]
    def initialBindings: Iterable[(String, Addr, V)] = List()
    def initialEnv: Iterable[(String, Addr)] = initialBindings.map({ case (name, a, _) => (name, a) })
    def initialStore: Iterable[(Addr, V)] = initialBindings.map({ case (_, a, v) => (a, v) })
}
