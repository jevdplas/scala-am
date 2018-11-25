package scalaam.core

trait Frame extends SmartHash

trait Semantics[Exp, Addr <: Address, V, T, C] {
    implicit val timestamp: Timestamp[T, C]
    implicit val lattice: Lattice[V]
    val allocator: Allocator[Addr, T, C]
    
    object Action {
        trait A
        case class Value(v: V, store: Store[Addr, V])                                           extends A
        case class Push(frame: Frame, e: Exp, env: Environment[Addr], store: Store[Addr, V])    extends A
        case class Eval(e: Exp, env: Environment[Addr], store: Store[Addr, V])                  extends A
        case class StepIn(fexp: Exp,
                          clo: (Exp, Environment[Addr]),
                          e: Exp,
                          env: Environment[Addr],
                          store: Store[Addr, V])
            extends A
        case class Err(err: Error) extends A
        
        // Action used when a new future needs to be created.
        case class NewFuture[TID <: ThreadIdentifier](tid: TID, tidv: V, e: Exp, env: Environment[Addr], store: Store[Addr, V]) extends A
        case class DerefFuture[TID <: ThreadIdentifier](tid: TID, store: Store[Addr, V]) extends A
        
        
        val None: Set[A] = Set.empty
        import scala.language.implicitConversions
        implicit def actionToSet(act: A): Set[A] = Set(act)
        implicit def fromMF(mf: MayFail[A, Error]): Set[A] = mf match {
            case MayFailSuccess(a) => Set(a)
            case MayFailError(errs) => errs.toSet.map(e => Err(e))
            case MayFailBoth(a, errs) => errs.toSet.map(e => Err(e)) ++ (Set(a))
        }
    }
    
    def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, V], t: T): Set[Action.A]
    def stepKont(v: V, frame: Frame, store: Store[Addr, V], t: T): Set[Action.A]
    def initialBindings: Iterable[(String, Addr, V)] = List()
    def initialEnv: Iterable[(String, Addr)] = initialBindings.map({ case (name, a, _) => (name, a) })
    def initialStore: Iterable[(Addr, V)] = initialBindings.map({ case (_, a, v) => (a, v) })
}
