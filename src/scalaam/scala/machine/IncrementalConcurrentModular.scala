package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.{Graph, LabeledTransition}
import scalaam.graph.Graph.GraphOps

import scala.core.MachineUtil
import scala.machine.ConcurrentModular.WrappedStore

class IncrementalConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    // ConcurrentModular can also be subclassed (which is easier), but this leads to issues with the type Transition in the signature of run.
    val concMod = new ConcurrentModular[Exp, A, V, T, TID](t, sem, allocator)
    import concMod._
    import concMod.seqAAM._
    
    type State   = concMod.State
    type Control = concMod.Control

    // Dependencies are now tracked on state basis instead of on thread basis.
    type StateJoinDeps  = Map[TID, Set[State]]
    type StateReadDeps  = Map[  A, Set[State]]
    type StateWriteDeps = Map[  A, Set[State]]
    
    type UnlabeledEdges = List[(State, State)]
    
    // Transitions are annotated with the iteration number of the outer loop in which they are generated (starting from one).
    override type Transition = LabeledTransition
    
    case class Deps(joined: StateJoinDeps, read: StateReadDeps, written: StateWriteDeps)
    
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    
        type Edges      = List[(State, Transition, State)]
    
        case class InnerLoopState(work: List[State], store: WStore, results: RetVals, visited: Set[State] = Set.empty,
                                  result: V = lattice.bottom, created: Created = Set.empty, effects: Effects = Set.empty,
                                  deps: Deps = Deps(Map.empty.withDefaultValue(Set.empty), Map.empty.withDefaultValue(Set.empty),
                                      Map.empty.withDefaultValue(Set.empty)),
                                  edges: UnlabeledEdges = List.empty)
        
        case class OuterLoopState(threads: Threads, work: List[State], deps: Deps, results: RetVals, store: WStore, edges: Edges)
    
        /** Innerloop like ConcurrentModular.run.innerLoop, except that now relations between effects and states are tracked. */
        @scala.annotation.tailrec
        def innerLoop(iState: InnerLoopState): InnerLoopState = {
            if (timeout.reached || iState.work.isEmpty) return iState
            innerLoop(iState.work.foldLeft(iState.copy(work = List())){case (iStateAcc, curState) =>
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
                    InnerLoopState(iStateAcc.work ++ successors, store.reset, iStateAcc.results, vis,
                                   lattice.join(iStateAcc.result, result.getOrElse(lattice.bottom)),
                                   iStateAcc.created ++ created, iStateAcc.effects ++ effects,
                                   Deps(iStateAcc.deps.joined ++ joined, read, written),
                                   iStateAcc.edges ++ successors.map((curState, _)))
                }
            })
        }
    
        /**
          * OuterLoop like ConcurrentModular.run.outerLoop, but upon reanalysis, a thread is not started from scratch again.
          * Added: iteration counter for edge annotation.
          */
        @scala.annotation.tailrec
        def outerLoop(oState: OuterLoopState, iteration: Int): OuterLoopState = {
            if (timeout.reached || oState.work.isEmpty) return oState
            outerLoop(oState.work.foldLeft(oState.copy(work = List())){case (oStateAcc, curState) =>
                val stid: TID = curState.tid
                val iState = innerLoop(InnerLoopState(List(curState), oStateAcc.store, oStateAcc.results))
                // Some new threads may have been spawned.
                val (todoCreated, newThreads): (Set[State], Threads) = iState.created.foldLeft((Set[State](), oStateAcc.threads)) {case ((createdAcc, threadsAcc), curState) =>
                    if (threadsAcc(curState.tid).contains(curState)) (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                    else (createdAcc + curState, threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState)))
                }
                // Add the newly found dependencies.
                val readDeps  = oStateAcc.deps.read    ++ iState.deps.read
                val writeDeps = oStateAcc.deps.written ++ iState.deps.written
                val joinDeps  = oStateAcc.deps.joined  ++ iState.deps.joined
                // Based on R/W and W/W conflicts, decide on the states that need re-evaluation.
                val todoEffects: List[State] = iState.deps.written.keySet.foldLeft(List[State]())((acc, addr) =>
                    if (oStateAcc.store.lookup(addr) == iState.store.lookup(addr)) acc else  acc ++ readDeps(addr).toList ++ writeDeps(addr).toList)
                // Calculate the thread's new return value. If it differs, some other threads joining this thread need re-evaluation.
                val retVal: V = lattice.join(oStateAcc.results(stid), iState.result)
                val todoJoined: List[State] = if (oStateAcc.results(stid) == retVal) List.empty else joinDeps(stid).toList
                val fromInterference: List[State] = todoEffects ++ todoJoined
                OuterLoopState(newThreads, oStateAcc.work ++ todoCreated ++ fromInterference, Deps(joinDeps, readDeps, writeDeps),
                    // All outgoing edges van states that need recomputation (are in fromInterference) are removed. Each edge that is added is annotated with the iteration number.
                    oStateAcc.results + (stid -> retVal), iState.store, oStateAcc.edges.filter(e => !fromInterference.contains(e._1)) ++ iState.edges.map(ue => (ue._1, LabeledTransition(iteration.toString), ue._2)))
            }, iteration + 1)
        }
    
        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(work: List[State], visited: Set[State], edges: Edges): Edges = {
            if (work.isEmpty) return edges
            if (visited.contains(work.head)) findConnectedStates(work.tail, visited, edges)
            else {
                val next = edges.filter(e => e._1 == work.head)
                findConnectedStates(work.tail ++ next.map(_._3), visited + work.head, edges ++ next)
            }
        }
    
        val cc      :          KAddr = HaltKontAddr
        val env     : Environment[A] = Environment.initial[A](sem.initialEnv)
        val control :        Control = concMod.ControlEval(program, env)
        val kstore  :         KStore = Store.empty[KAddr, Set[Kont]](t)
        val time    :              T = timestamp.initial("")
        val tid     :            TID = allocator.allocate(program, time)
        val state   :          State = concMod.State(tid, control, cc, time, kstore)
        val threads :        Threads = Map(tid -> Set(state)).withDefaultValue(Set.empty)
        val vstore  :         VStore = Store.initial[A, V](t, sem.initialStore)(lattice)
        val wstore  :         WStore = WrappedStore[A, V](vstore)(lattice)
        val oState  : OuterLoopState = OuterLoopState(threads,                              // Threads.
                                       List(state),                                         // Worklist.
                                       Deps(Map.empty.withDefaultValue(Set.empty),          // Join dependencies.
                                            Map.empty.withDefaultValue(Set.empty),          // Read dependencies.
                                            Map.empty.withDefaultValue(Set.empty)),         // Write dependencies.
                                            Map.empty.withDefaultValue(lattice.bottom),     // Return values.
                                       wstore,                                              // Store.
                                       List.empty)                                          // Graph edges.
        
        val result: OuterLoopState = outerLoop(oState, 1)
        Graph[G, State, Transition].empty.addEdges(findConnectedStates(result.threads.values.flatten.toList, Set(), result.edges))
    }
}