package scala.bench

import scalaam.core.StoreType.StoreType
import scalaam.core._
import scala.machine.ConcurrentModularRec

/**
  * Implementation of a concurrent modular machine that is incremental in the construction of the inner-loop (intra-modular) results.
  * This is accomplished by a change in dependency tracking, so that dependencies are to States and not to Threads, i.e., the tracking
  * of dependencies now uses a finer granularity.
  */
class IncrementalConcurrentModularRec[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends ConcurrentModularRec[Exp, A, V, T, TID](t, sem, allocator) {
    
    // Dependencies are now tracked on state basis instead of on thread basis.
    type StateJoinDeps  = Map[TID, Set[State]]
    type StateReadDeps  = Map[  A, Set[State]]
    type StateWriteDeps = Map[  A, Set[State]]
    
    override def run[G](program: Exp, timeout: Timeout.T, name: String): (Int, Int, Int) = (-1, -1, -1)
}