package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.Graph

class IncrementalConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends ConcurrentModular[Exp, A, V, T, TID](t, sem, allocator) {

    // Dependencies are now tracked on state basis instead of on thread basis.
    type StateJoinDeps  = Map[TID, Set[State]]
    type StateReadDeps  = Map[  A, Set[State]]
    type StateWriteDeps = Map[  A, Set[State]]
    
    case class Deps(joined: StateJoinDeps, read: StateReadDeps, written: StateWriteDeps)
    
    override def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    
        case class InnerLoopState(work: List[State], visited: Set[State], store: WStore, result: V, created: Created,
                                  effects: Effects, deps: Deps, edges: Edges, results: RetVals)
    
        /**
          * Innerloop like ConcurrentModular.run, except that now relations between effects and states are tracked.
          */
        @scala.annotation.tailrec
        def innerloop(iState: InnerLoopState): InnerLoopState = {
            if (timeout.reached || iState.work.isEmpty) return iState
            innerloop(iState.work.foldLeft(iState.copy(work = List())){case (iStateAcc, curState) =>
                if (iStateAcc.visited.contains(curState)) iStateAcc
                else {
                    val StepResult(successors, created, _, result, effects, store: WStore) = curState.step(iStateAcc.store, iStateAcc.results)
                    val (read, written, joined) = effects.foldLeft((iStateAcc.deps.read, iStateAcc.deps.written, iStateAcc.deps.joined))
                        {case (acc@(r, w, j), eff) => eff match {
                            case     JoinEff(tid: TID@unchecked) => (r, w, j + (tid -> (j(tid) + curState)))
                            case  ReadAddrEff(addr: A@unchecked) => (r + (addr -> (r(addr) + curState)), w, j)
                            case WriteAddrEff(addr: A@unchecked) => (r, w + (addr -> (w(addr) + curState)), j)
                            case _ => acc
                            }
                        }
                    val vis = if (store.updated) Set.empty[State] else iStateAcc.visited + curState
                    InnerLoopState(iStateAcc.work ++ successors, vis, store,
                                   lattice.join(iStateAcc.result, result.getOrElse(lattice.bottom)),
                                   iStateAcc.created ++ created, iStateAcc.effects ++ effects,
                                   Deps(iStateAcc.deps.joined ++ joined, read, written),
                                   iStateAcc.edges ++ successors.map((curState, empty, _)),
                                   iStateAcc.results)
                }
            })
        }
        
    }
}