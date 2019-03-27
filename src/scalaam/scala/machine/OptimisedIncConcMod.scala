package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.{BaseTransition, Graph}
import scalaam.graph.Graph.GraphOps

import scala.machine.ConcurrentModular.WrappedStore

class OptimisedIncConcMod [Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends IncrementalConcurrentModular[Exp, A, V, T, TID](t, sem, allocator) {
    
    import seqAAM._
    
    type TStore = TimestampedStore[A, V]
    
    override def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        
        type Edges          = Map[State, Set[(Transition, State)]]
        type GraphEdges     = List[(State, Transition, State)]
    
        case class InnerLoopState(work: List[State], store: WStore, results: RetVals, visited: Set[State] = Set.empty,
                                  result: V = lattice.bottom, created: Created = Set.empty, effects: Effects = Set.empty,
                                  deps: Deps = Deps(Map.empty.withDefaultValue(Set.empty), Map.empty.withDefaultValue(Set.empty),
                                      Map.empty.withDefaultValue(Set.empty)),
                                  edges: UnlabeledEdges = Map.empty) extends SmartHash
    
        case class OuterLoopState(threads: Threads, work: List[State], deps: Deps, results: RetVals, store: WStore, edges: Edges) extends SmartHash
    
        /** Optimised innerLoop: the loop will not explore paths that have to be reanalysed anyway (i.e. after reading a bottom return value). */
        @scala.annotation.tailrec
        def innerLoop(iState: InnerLoopState): InnerLoopState = {
            if (timeout.reached || iState.work.isEmpty) return iState
            innerLoop(iState.work.foldLeft(iState.copy(work = List())){case (iStateAcc, curState) =>
                if (iStateAcc.visited.contains(curState)) iStateAcc
                else {
                    val StepResult(successors, created, _, result, effects, store: WStore) = curState.step(iStateAcc.store, iStateAcc.results)
                    // The bottomRead flag indicates whether a bottom result value was read.
                    val (read, written, joined, bottomRead) = effects.foldLeft((iStateAcc.deps.read, iStateAcc.deps.written, iStateAcc.deps.joined, false))
                        {case (acc@(r, w, j, br), eff) => eff match {
                            case     JoinEff(tid: TID@unchecked) => (r, w, j + (tid -> (j(tid) + curState)), br || (iState.results(tid) == lattice.bottom))
                            case  ReadAddrEff(addr: A@unchecked) => (r + (addr -> (r(addr) + curState)), w, j, br)
                            case WriteAddrEff(addr: A@unchecked) => (r, w + (addr -> (w(addr) + curState)), j, br)
                            case _ => acc
                        }
                    }
                    val vis = if (store.updated) Set.empty[State] else iStateAcc.visited + curState
                    // Do not explore a path if it will have to be reanalysed later anyway (because of reading an unanalysed thread's return value).
                    // Fixme: We are now throwing away ALL successor states, although only some of them may be the result from reading bottom.
                    // Fixme: Hence, we may be throwing away too many successor states. This may depend on the precision of the thread identifiers, but
                    // Fixme: in the end, we are sure the entire graph is explored indeed, although more iterations of the outer loop may be needed.
                    val suc = if (bottomRead) Set.empty[State] else successors
                    InnerLoopState(iStateAcc.work ++ suc, store.reset, iStateAcc.results, vis,
                                   lattice.join(iStateAcc.result, result.getOrElse(lattice.bottom)),
                                   iStateAcc.created ++ created, iStateAcc.effects ++ effects,
                                   Deps(iStateAcc.deps.joined ++ joined, iStateAcc.deps.read ++ read, iStateAcc.deps.written ++ written),
                                   iStateAcc.edges + (curState -> suc))
                }
            })
        }
    
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
                    oStateAcc.results + (stid -> retVal), iState.store, oStateAcc.edges -- fromInterference ++ iState.edges.mapValues(set => set.map((BaseTransition(iteration.toString), _))))
            }, iteration + 1)
        }
    
        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(work: List[State], visited: Set[State], edges: GraphEdges): GraphEdges = {
            if (work.isEmpty) return edges
            if (visited.contains(work.head)) findConnectedStates(work.tail, visited, edges)
            else {
                val next = edges.filter(e => e._1 == work.head)
                findConnectedStates(work.tail ++ next.map(_._3), visited + work.head, edges ++ next)
            }
        }
    
        val cc      :          KAddr = HaltKontAddr
        val env     : Environment[A] = Environment.initial[A](sem.initialEnv)
        val control :        Control = ControlEval(program, env)
        val kstore  :         KStore = Store.empty[KAddr, Set[Kont]](t)
        val time    :              T = timestamp.initial("")
        val tid     :            TID = allocator.allocate(program, time)
        val state   :          State = State(tid, control, cc, time, kstore)
        val threads :        Threads = Map(tid -> Set(state)).withDefaultValue(Set.empty)
        val vstore  :         VStore = Store.initial[A, V](t, sem.initialStore)(lattice)
        val wstore  :         WStore = WrappedStore[A, V](vstore)(lattice)
        val oState  : OuterLoopState = OuterLoopState(threads,                                             // Threads.
                                                      List(state),                                         // Worklist.
                                                      Deps(Map.empty.withDefaultValue(Set.empty),          // Join dependencies.
                                                           Map.empty.withDefaultValue(Set.empty),          // Read dependencies.
                                                           Map.empty.withDefaultValue(Set.empty)),         // Write dependencies.
                                                      Map.empty.withDefaultValue(lattice.bottom),          // Return values.
                                                      wstore,                                              // Store.
                                                      Map.empty)                                           // Graph edges.
    
        val result: OuterLoopState = outerLoop(oState, 1)
        Graph[G, State, Transition].empty.addEdges(findConnectedStates(result.threads.values.flatten.toList, Set(), result.edges.toList.flatMap(t => t._2.map(e => (t._1, e._1, e._2)))))
    }
}

/**
  * StoreWrapper that contains a version number indicating the version of the store. Analogous to WrappedStore in ConcurrentModular.scala.
  * @param store   The store that is wrapped.
  * @param version The version number that is associated with the wrapped store.
  * @see scala.machine.ConcurrentModular.WrappedStore for implementation details.
  */
case class TimestampedStore[A <: Address, V](store: Store[A, V], version: Int = 0)(implicit val lattice: Lattice[V]) extends Store[A, V] {
    def                       keys: Iterable[A] = store.keys
    def forall(p: ((A, V)) => Boolean): Boolean = store.forall(p)
    def    subsumes(that: Store[A, V]): Boolean = store.subsumes(that)
    def                 lookup(a: A): Option[V] = store.lookup(a)
    def       lookupMF(a: A): MayFail[V, Error] = store.lookupMF(a)
    def         extend(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.extend(a, v))
    def         update(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.update(a, v))
    def updateOrExtend(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.updateOrExtend(a, v))
    def    join(that: Store[A, V]): Store[A, V] = verifyMerge(that)
    
    private def verifyChange(a: A, v: V, fun: (A, V) => Store[A, V]): Store[A, V] = {
        store.lookup(a) match {
            case None => TimestampedStore(fun(a, v), version + 1)
            case Some(v1) =>
                val store_ : Store[A, V] = fun(a, v)
                val v2 : V = store_.lookup(a).getOrElse(throw new Exception(s"Condition failure: missing required binding of $a in store $store_."))
                TimestampedStore(store_, if (lattice.lteq(v2, v1)) version + 1 else version)
        }
    }
    
    private def verifyMerge(that: Store[A, V]): Store[A, V] = {
        if (store.keys != that.keys)
            TimestampedStore(store.join(that), version + 1)
        else {
            for (key <- store.keys) {
                if (!lattice.lteq(that.lookup(key).get, store.lookup(key).get))
                    return TimestampedStore(store.join(that), version + 1)
            }
            TimestampedStore(store.join(that), version)
        }
    }
}