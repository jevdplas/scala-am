package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.{BaseTransition, Graph}
import scalaam.graph.Graph.GraphOps

/**
  * Implementation of a concurrent modular machine that is incremental in the construction of the inner-loop (intra-modular) results.
  * This is accomplished by a change in dependency tracking, so that dependencies are to States and not to Threads, i.e., the tracking
  * of dependencies now uses a finer granularity.
  */
class IncrementalConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends ConcurrentModular[Exp, A, V, T, TID](t, sem, allocator) {
    
    import seqAAM._

    // Dependencies are now tracked on state basis instead of on thread basis.
    type StateJoinDeps  = Map[TID, Set[State]]
    type StateReadDeps  = Map[  A, Set[State]]
    type StateWriteDeps = Map[  A, Set[State]]
    
    var theLabels: List[(Int, Map[Int, Int])] = List()
    
    /** Class collecting the dependencies of all threads. */
    case class Deps(joined: StateJoinDeps, read: StateReadDeps, written: StateWriteDeps)
    
    /** Class containing bookkeeping information for the inner loop of a thread. All arguments except the first three are optional. */
    case class InnerLoopState(work: List[State], store: VStore, results: RetVals, visited: Set[State] = Set.empty,
                              result: V = lattice.bottom, created: Created = Set.empty, effects: Effects = Set.empty,
                              deps: Deps = Deps(Map.empty.withDefaultValue(Set.empty), Map.empty.withDefaultValue(Set.empty),
                                  Map.empty.withDefaultValue(Set.empty)),
                              edges: UnlabeledEdges = Map.empty) extends SmartHash
    
    /** Class containing bookkeeping information for the outer loop. Contains a.o. the global store. */
    case class OuterLoopState(threads: Threads, work: List[State], deps: Deps, results: RetVals, store: VStore, edges: Edges) extends SmartHash
    
    /** Innerloop like ConcurrentModular.run.innerLoop, except that now relations between effects and states are tracked. */
    @scala.annotation.tailrec
    final def innerLoop(timeout: Timeout.T, iState: InnerLoopState): InnerLoopState = {
        if (timeout.reached || iState.work.isEmpty) return iState
        innerLoop(timeout, iState.work.foldLeft(iState.copy(work = List())){case (iStateAcc, curState) =>
            if (iStateAcc.visited.contains(curState)) iStateAcc
            else {
                val StepResult(successors, created, result, effects, store: VStore) = curState.step(iStateAcc.store, iStateAcc.results)
                val (read, written, joined) = effects.foldLeft((iStateAcc.deps.read, iStateAcc.deps.written, iStateAcc.deps.joined))
                {case (acc@(r, w, j), eff) => eff match {
                    case     JoinEff(tid: TID@unchecked) => (r, w, j + (tid -> (j(tid) + curState)))
                    case  ReadAddrEff(addr: A@unchecked) => (r + (addr -> (r(addr) + curState)), w, j)
                    case WriteAddrEff(addr: A@unchecked) => (r, w + (addr -> (w(addr) + curState)), j)
                    case _ => acc
                }
                }
                val vis = if (store.asInstanceOf[DeltaStore[A, V]].updated.nonEmpty) Set.empty[State] else iStateAcc.visited + curState
                InnerLoopState(iStateAcc.work ++ successors, store.asInstanceOf[DeltaStore[A, V]].clearUpdated, iStateAcc.results, vis,
                    lattice.join(iStateAcc.result, result.getOrElse(lattice.bottom)),
                    iStateAcc.created ++ created, iStateAcc.effects ++ effects,
                    Deps(joined, read, written),
                    // iStateAcc.edges + (curState -> (iStateAcc.edges.getOrElse(curState, Set.empty) ++ successors)))
                    iStateAcc.edges + (curState -> successors))
            }
        })
    }
    
    /**
      * OuterLoop like ConcurrentModular.run.outerLoop, but upon reanalysis, a thread is not started from scratch again.
      * Added: iteration counter for edge annotation.
      */
    @scala.annotation.tailrec
    final def outerLoop(timeout: Timeout.T, oState: OuterLoopState, iteration: Int = 1): OuterLoopState = {
        if (timeout.reached || oState.work.isEmpty) return oState
        outerLoop(timeout, oState.work.foldLeft(oState.copy(work = List())){case (oStateAcc, curState) =>
            val stid: TID = curState.tid
            val iState = innerLoop(timeout, InnerLoopState(List(curState), oStateAcc.store, oStateAcc.results))
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
                if (oStateAcc.store.lookup(addr) == iState.store.lookup(addr)) acc else acc ++ readDeps(addr).filter(_.tid != stid) ++ writeDeps(addr).filter(_.tid != stid))
            // Calculate the thread's new return value. If it differs, some other threads joining this thread need re-evaluation.
            val retVal: V = lattice.join(oStateAcc.results(stid), iState.result)
            val todoJoined: Set[State] = if (oStateAcc.results(stid) == retVal) Set.empty else joinDeps(stid)
            val fromInterference: Set[State] = todoJoined ++ todoEffects // Use a set to suppress duplicates.
            OuterLoopState(newThreads, oStateAcc.work ++ todoCreated ++ fromInterference, Deps(joinDeps, readDeps, writeDeps),
                // All outgoing edges of states that need recomputation (are in fromInterference) are removed. Each edge that is added is annotated with the iteration number.
                oStateAcc.results + (stid -> retVal), iState.store, oStateAcc.edges -- fromInterference ++ iState.edges.mapValues(set => set.map((BaseTransition(iteration.toString), _))))
        }, iteration + 1)
    }
    
    override def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    
        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(timeout: Timeout.T, work: List[State], edges: Edges, visited: Set[State] = Set.empty, acc: GraphEdges = List.empty): GraphEdges = {
            if (timeout.reached || work.isEmpty) return acc
            if (visited.contains(work.head)) findConnectedStates(timeout, work.tail, edges, visited, acc)
            else {
                val head = work.head
                val next = edges(head)
                // Prepend the edges and work upfront the respective lists (assume next to be much shorter than work/acc).
                findConnectedStates(timeout, next.map(_._2).toList ++ work.tail, edges, visited + head, next.map(t => (head, t._1, t._2)).toList ++ acc)
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
        val oState  : OuterLoopState = OuterLoopState(threads,                              // Threads.
                                       List(state),                                         // Worklist.
                                       Deps(Map.empty.withDefaultValue(Set.empty),          // Join dependencies.
                                            Map.empty.withDefaultValue(Set.empty),          // Read dependencies.
                                            Map.empty.withDefaultValue(Set.empty)),         // Write dependencies.
                                            Map.empty.withDefaultValue(lattice.bottom),     // Return values.
                                       vstore,                                              // Store.
                                       Map.empty)                                           // Graph edges.
    
        val result: OuterLoopState = outerLoop(timeout, oState)
        theStore = result.store
        // After running the result, possibly unreachable edges may need to be filtered out.
        Graph[G, State, Transition].empty.addEdges(findConnectedStates(timeout, result.threads.values.flatten.toList, result.edges))
    
    }
    
    def runWithLabels[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    
        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(timeout: Timeout.T, tid: TID, work: List[State], edges: Edges, visited: Set[State] = Set.empty, acc: GraphEdges = List.empty, labels: Map[Int, Int] = Map().withDefaultValue(0)): (GraphEdges, Map[Int, Int]) = {
            if (timeout.reached || work.isEmpty) return (acc, labels)
            if (visited.contains(work.head)) findConnectedStates(timeout, tid, work.tail, edges, visited, acc, labels)
            else {
                val head = work.head
                val next = edges(head)
                val lbs = next.foldLeft(labels){(lbs, e) =>
                    val i = e._1.l.toInt
                    lbs + (i -> (lbs(i) + 1))}
                // Prepend the edges and work upfront the respective lists (assume next to be much shorter than work/acc).
                findConnectedStates(timeout, tid, next.map(_._2).toList ++ work.tail, edges, visited + head, next.map(t => (head, t._1, t._2)).toList ++ acc, lbs)
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
        val oState  : OuterLoopState = OuterLoopState(threads,                              // Threads.
            List(state),                                         // Worklist.
            Deps(Map.empty.withDefaultValue(Set.empty),          // Join dependencies.
                Map.empty.withDefaultValue(Set.empty),          // Read dependencies.
                Map.empty.withDefaultValue(Set.empty)),         // Write dependencies.
            Map.empty.withDefaultValue(lattice.bottom),     // Return values.
            vstore,                                              // Store.
            Map.empty)                                           // Graph edges.
    
        val result: OuterLoopState = outerLoop(timeout, oState)
        val (edg, labels): (GraphEdges, List[(TID, Map[Int, Int])]) =
            result.threads.keySet.foldLeft((List[(State, Transition, State)](),
                List[(TID, Map[Int, Int])]()))
            { case ((edg, lab), td) =>
                val (e, l): (GraphEdges, Map[Int, Int]) = findConnectedStates(timeout, td, result.threads(td).toList, result.edges)
                (edg ++ e, (td, l) :: lab)
            }
        theLabels = labels.foldLeft((List[(Int, Map[Int, Int])](), 0)){case ((acc, n), (_, mp)) => ((n, mp) :: acc, n + 1)}._1
        Graph[G, State, Transition].empty.addEdges(edg)
        
    }
    
}
