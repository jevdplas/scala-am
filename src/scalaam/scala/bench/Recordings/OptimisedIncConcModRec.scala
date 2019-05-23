package scala.bench.Recordings

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.BaseTransition

import scala.machine.TimestampedStore

/**
  * Implementation of an optimised version of the IncrementalConcurrentModular machine. Two optimisations are implemented: <br>
  * <ul>
  *     <li><s> InnerLoop abortion when it is known that the next states will need to be reanalysed. his is the case when a bottom
  *          return value of another thread is read. Note that in the current implementation, all successors are thrown away, which
  *          may cause more iterations of OuterLoop.</s><br< Implemented in all modular machines. </li>
  *     <li> Caching of the visited set. Instead of restarting the inner loop just from a specific state with an empty visited
  *          set, it may be possible also to use the old set. However, when the store has changed in the meantime, this is not
  *          possible. Hence, the store contains version numbers that can be compared to see whether it has changed.</li>
  * </ul>
  * <b>Important:</b> note that the implementation is incremental in the construction of the inner-loop (intra-modular) results and <i>not</i>
  * with regard to source code changes.
  *
  * @see scala.machine.IncrementalConcurrentModular
  */
class OptimisedIncConcModRec[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])(
    override implicit val timestamp: Timestamp[T, Exp],
    override implicit val lattice: Lattice[V])
    extends IncrementalConcurrentModularRec[Exp, A, V, T, TID](t, sem, allocator) {
    
    import recorder._
    import seqAAM._
    
    // Dependencies are now tracked on state basis instead of on thread basis.  However, unlike the unoptimised machine,
    // the visited set is now stored along with the state. This set can be used to restart the inner loop with, provided
    // that the store has not changed (this is why a store version number is stored too). By being able to recover an old
    // visited set, it may be possible to further reduce the number of states to be explored.
    type WorkItem = (State, Set[State], Int)
    type ExtendedStateJoinDeps  = Map[TID, Set[WorkItem]]
    type ExtendedStateReadDeps  = Map[  A, Set[WorkItem]]
    type ExtendedStateWriteDeps = Map[  A, Set[WorkItem]]
    
    type TStore = TimestampedStore[A, V]
    
    override def run[G](program: Exp, timeout: Timeout.T, name: String): (Int, Int, Int) = {
        
        // Reuse of visited sets.
        var reuse = -1 // The first invocation of outerLoop will increase this to 0.
        var nonreuse = 0
        var size  = 0
        
        /** Class collecting the dependencies of all threads. */
        case class Deps(joined: ExtendedStateJoinDeps, read: ExtendedStateReadDeps, written: ExtendedStateWriteDeps)
        
        /** Class containing bookkeeping information for the inner loop of a thread. All arguments except the first three are optional. */
        case class InnerLoopState(work: List[State], store: TStore, results: RetVals, visited: Set[State] = Set.empty,
                                  result: V = lattice.bottom, created: Created = Set.empty, effects: Effects = Set.empty,
                                  deps: Deps = Deps(Map.empty.withDefaultValue(Set.empty), Map.empty.withDefaultValue(Set.empty),
                                      Map.empty.withDefaultValue(Set.empty)),
                                  edges: UnlabeledEdges = Map.empty) extends SmartHash
        
        /** Class containing bookkeeping information for the outer loop. Contains a.o. the global store. */
        case class OuterLoopState(threads: Threads, work: List[WorkItem], deps: Deps, results: RetVals, store: TStore, edges: Edges) extends SmartHash
        
        /** Optimised innerLoop: the loop will not explore paths that have to be reanalysed anyway (i.e. after reading a bottom return value). */
        @scala.annotation.tailrec
        def innerLoop(iState: InnerLoopState): InnerLoopState = {
            if (timeout.reached || iState.work.isEmpty) return iState
            innerLoop(iState.work.foldLeft(iState.copy(work = List())){case (iStateAcc, curState) =>
                if (iStateAcc.visited.contains(curState)) iStateAcc
                else {
                    val StepResult(successors, created, result, effects, store: TStore) = curState.step(iStateAcc.store, iStateAcc.results)
                    // The bottomRead flag indicates whether a bottom result value was read. The visitedset and store version number are also saved, as
                    // they may be used when analysis is restarted. When the store has been modified already in this step, we immediately remember an
                    // empty visited set.
                    val (read, written, joined) = effects.foldLeft((iStateAcc.deps.read, iStateAcc.deps.written, iStateAcc.deps.joined))
                    {case (acc@(r, w, j), eff) => eff match {
                        case     JoinEff(tid: TID@unchecked) => (r, w, j + (tid -> (j(tid) + ((curState, iStateAcc.visited, iStateAcc.store.version)))))
                        case  ReadAddrEff(addr: A@unchecked) => (r + (addr -> (r(addr) + ((curState, iStateAcc.visited, iStateAcc.store.version)))), w, j)
                        case WriteAddrEff(addr: A@unchecked) => (r, w + (addr -> (w(addr) + ((curState, iStateAcc.visited, iStateAcc.store.version)))), j)
                        case _ => acc
                    }
                    }
                    // Vis cannot be used for caching since it may contain curState. This way, an innerLoop started from curState and vis would stop immediately.
                    val vis = if (store.version != iStateAcc.store.version) Set.empty[State] else iStateAcc.visited + curState
                    InnerLoopState(iStateAcc.work ++ successors, store, iStateAcc.results, vis,
                        lattice.join(iStateAcc.result, result.getOrElse(lattice.bottom)),
                        iStateAcc.created ++ created, iStateAcc.effects ++ effects,
                        Deps(joined, read, written),
                        iStateAcc.edges + (curState -> successors))
                }
            })
        }
        
        /** Outer loop of the fixed-point computation. */
        @scala.annotation.tailrec
        def outerLoop(oState: OuterLoopState, iteration: Int = 1): OuterLoopState = {
            if (timeout.reached || oState.work.isEmpty) return oState
            outerLoop(oState.work.foldLeft(oState.copy(work = List())){case (oStateAcc, (curState, prevVisisted, version)) =>
                val stid: TID = curState.tid
                if (version == oStateAcc.store.version) {
                    reuse += 1
                    size  += prevVisisted.size
                } else {
                    nonreuse += 1
                }
                // Analysis of a single thread. When this thread is actually reanalysed, it may be possible to reuse the visited set given that the store is not changed.
                val iState = innerLoop(InnerLoopState(List(curState),
                    oStateAcc.store,
                    oStateAcc.results,
                    if (version == oStateAcc.store.version) prevVisisted else Set.empty))
                // Some new threads may have been spawned.
                val (todoCreated, newThreads): (Set[WorkItem], Threads) = iState.created.foldLeft((Set[WorkItem](), oStateAcc.threads)) {case ((createdAcc, threadsAcc), curState) =>
                    if (threadsAcc(curState.tid).contains(curState)) (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                    // A new thread will be evaluated with an empty visited set anyway, so add store version number 0.
                    else (createdAcc + ((curState, Set.empty, 0)), threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState)))
                }
                // Add the newly found dependencies.
                iState.deps.read.foreach(d => recordedRead.record(stid, (d._1, oStateAcc.store.lookup(d._1).getOrElse(lattice.bottom))))
                iState.deps.written.foreach(d => recordedWrite.record(stid, (d._1, iState.store.lookup(d._1).getOrElse(lattice.bottom))))
                val readDeps  = oStateAcc.deps.read    ++ iState.deps.read
                val writeDeps = oStateAcc.deps.written ++ iState.deps.written
                val joinDeps  = oStateAcc.deps.joined  ++ iState.deps.joined
                // Based on R/W and W/W conflicts, decide on the states that need re-evaluation.
                val todoEffects: List[WorkItem] = iState.deps.written.keySet.foldLeft(List[WorkItem]())((acc, addr) =>
                    if (oStateAcc.store.lookup(addr) == iState.store.lookup(addr)) acc else acc ++ readDeps(addr).filter(_._1.tid != stid) ++ writeDeps(addr).filter(_._1.tid != stid))
                // Calculate the thread's new return value. If it differs, some other threads joining this thread need re-evaluation.
                val retVal: V = lattice.join(oStateAcc.results(stid), iState.result)
                val todoJoined: Set[WorkItem] = if (oStateAcc.results(stid) == retVal) Set.empty else joinDeps(stid)
                val fromInterference: Set[WorkItem] = todoJoined ++ todoEffects // Use a set to suppress duplicates.
                OuterLoopState(newThreads, oStateAcc.work ++ todoCreated ++ fromInterference, Deps(joinDeps, readDeps, writeDeps),
                    // All outgoing edges of states that need recomputation (are in fromInterference) are removed. Each edge that is added is annotated with the iteration number.
                    oStateAcc.results + (stid -> retVal), iState.store, oStateAcc.edges -- fromInterference.map(_._1) ++ iState.edges.mapValues(set => set.map((BaseTransition(iteration.toString), _))))
            }, iteration + 1)
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
        val tstore  :         TStore = TimestampedStore[A, V](vstore)(lattice)
        val oState  : OuterLoopState = OuterLoopState(threads,   // Threads.
            List((state, Set.empty, tstore.version)),            // Worklist.
            Deps(Map.empty.withDefaultValue(Set.empty),          // Join dependencies.
                Map.empty.withDefaultValue(Set.empty),           // Read dependencies.
                Map.empty.withDefaultValue(Set.empty)),          // Write dependencies.
            Map.empty.withDefaultValue(lattice.bottom),          // Return values.
            tstore,                                              // Store.
            Map.empty)                                           // Graph edges.
        
        recorder.reset()
        outerLoop(oState)
        recorder.outputRecorded(name + ".inceffs")
        (reuse, nonreuse, size)
    }
}