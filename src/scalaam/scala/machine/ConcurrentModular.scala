package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.machine.AAM

import scalaam.graph.Graph.GraphOps
import scalaam.graph._

import scala.core.MachineUtil

class ConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}
    import scala.machine.ConcurrentModular.WrappedStore
    
    /** Certain parts of this AAM will be reused. */
    val seqAAM = new AAM[Exp, A, V, T](t, sem)
    import seqAAM._
    
    /** Various type declarations. */
    type KAddr      = KA
    
    type VStore     = Store[A, V]
    type WStore     = WrappedStore[A, V]
    type KStore     = Store[KAddr, Set[Kont]]
    
    type Created    = Set[State]
    type Successors = Set[State]
    type Joined     = Set[TID]
   
    type Threads    = Map[TID, Set[State]]
    type RetVals    = Map[TID, V]
    type JoinDeps   = Map[TID, Set[TID]]
    type ReadDeps   = Map[TID, Set[A]]
    type WriteDeps  = Map[TID, Set[A]]
    
    type Edges      = List[(State, Transition, State)]
    
    /** Class used to return all information resulting from stepping this state. */
    case class StepResult(successors: Successors, created: Created, joined: Joined, result: Option[V], effects: Effects, store: VStore) {
        // Adds the accumulator. Important: keeps the store of "this".
        def merge(acc: StepResult): StepResult =
            StepResult(successors ++ acc.successors,
                created ++ acc.created,
                joined ++ acc.joined,
                Option.empty,
                effects ++ acc.effects,
                store) // Important: keeps the store of "this".
    }
    
    /**
      * The execution state of one process (thread), containing its identifier and running information.
      * The variable store is shared among all threads; the continuation store is not.
      *
      * @param tid     The thread identifier corresponding to this context.
      * @param control The control component of the thread.
      * @param cc      The address of the current continuation.
      * @param time    A timestamp.
      * @param kstore  A continuation store.
      */
    case class State(tid: TID, control: Control, cc: KAddr, time: T, kstore: KStore) extends GraphElement with SmartHash {
        override def toString: String = control.toString
    
        override def label: String = toString
    
        override def color: Color = control match {
            case ControlEval(_, _)        => Colors.Blue
            case ControlKont(_) if halted => Colors.Grass
            case ControlKont(_)           => Colors.Yellow
            case ControlError(_)          => Colors.Red
        }
        
        override def metadata = GraphMetadataMap(Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("concMod")))
        
        /** Indicates whether this state is a final state. */
        def halted: Boolean = (cc, control) match {
            case (HaltKontAddr, ControlKont(_)) => true
            case (_, ControlError(_)) => true
            case _ => false
        }
        
        /** Indicates whether this state represents an error. */
        def errored: Boolean = control match {
            case ControlError(_) => true
            case _ => false
        }
        
        /** Helper function to create new results easily without having to write all fields explicitly. */
        private def newResult(successor: State, effects: Effects, store: VStore,
                              created: Created = Set.empty, joined: Joined = Set.empty, result: Option[V] = Option.empty): StepResult = {
            StepResult(Set(successor), created, joined, result, effects, store)
        }
        
        /**
          * Executes an action on this state and returns the result as well as other bookkeeping information in the form of a stepResults instance.
          * @param action  The action to perform on this state.
          * @param old     The store that was used to obtain the action.
          * @param cc      The address of the current continuation. May differ from the instance variable cc stored in this state (e.g. after a frame has been popped of the stack).
          * @param results A map of TIDs to values, used to retrieve the return values of other processes.
          * @return Returns a stepResult containing all successor states of this states after performing the actions
          *         as well as bookkeeping information and the resulting store.
          */
        private def act(action: Act, time: T, old: VStore, cc: KAddr, results: RetVals): StepResult = action match {
            // The semantics reached a value => continue with this value.
            case Value(v, store, effs) => newResult(State(tid, ControlKont(v), cc, timestamp.tick(time), kstore), effs, store)
            // A frame needs to be pushed on the stack and the evaluation needs to continue by evaluating 'e'.
            case Push(frame, e, env, store, effs) =>
                val cc_ = KontAddr(e, time)
                newResult(State(tid, ControlEval(e, env), cc_, timestamp.tick(time), kstore.extend(cc_, Set(Kont(frame, cc)))), effs, store)
            // The expression 'e' needs to be evaluated in the given environment.
            case Eval(e, env, store, effs) => newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time), kstore), effs, store)
            // The evaluation steps into a function body. Same than Eval, except that the function is used to tick the timestamp.
            case StepIn(f, _, e, env, store, effs) => newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time, f), kstore), effs, store)
            // The semantics reached an error, which needs to be returned.
            case Err(e) => newResult(State(tid, ControlError(e), cc, timestamp.tick(time), kstore), Set.empty, old)
            // A new process is spawn by the semantics. The machine allocates a new TID and records the state of the new process.
            case NewFuture(ftid: TID@unchecked, tidv, fst, frame: Frame@unchecked, env, store, effs) =>
                val cc_ = KontAddr(fst, time)
                val newPState = State(ftid, ControlEval(fst, env), cc_, timestamp.initial(ftid.toString), Store.empty[KA, Set[Kont]](t).extend(cc_, Set(Kont(frame, HaltKontAddr))))
                val curPState = State(tid, ControlKont(tidv), cc, timestamp.tick(time), kstore)
                StepResult(Set(curPState), Set(newPState), Set.empty, Option.empty, effs ++ Effects.spawn(ftid), store)
            // The semantics wants to read a value from another thread, which needs to be looked up in the 'results' map.
            case DerefFuture(ftid: TID@unchecked, store, effs) =>
                StepResult(results.get(ftid).map(v => State(tid, ControlKont(v), cc, timestamp.tick(time), kstore)).toSet, Set.empty, Set(ftid), Option.empty, effs ++ Effects.join(ftid), store)
            // An unknown action has been returned. Should not happen, therefore this is an error.
            case a => throw new Exception(s"Unsupported action: $a.\n")
        }
    
        /**
          * Produces the states following this state by applying the given actions successively, thereby updating the store.
          * @param actions The set of actions to perform.
          * @param old     The store that was used to obtain the actions.
          * @param cc      The address of the current continuation. May differ from the instance variable cc stored in this state.
          * @param results A map of TIDs to values, used to retrieve the return values of other processes.
          * @return Returns a stepResult containing all successor states and bookkeeping information, as well as the final store.
          */
        private def next(actions: Set[Act], old: VStore, cc: KAddr, results: RetVals): StepResult = {
            val init: StepResult = StepResult(Set.empty, Set.empty, Set.empty, Option.empty, Set.empty, old)
            actions.foldLeft(init)((acc, curAction) => act(curAction, time, acc.store, cc, results).merge(acc))
        }
        
        /**
          * Decides on what to do by looking at the control component of the state and executes the according actions
          * if necessary.
          * @param store The store to use while stepping this state.
          * @return A tuple containing of:
          *         * the successor states of this state;
          *         * the processes created by this thread;
          *         * the processes of which this thread reads the result;
          *         * a possible result value (filled when the process finishes execution);
          *         * the effects generated during this step of execution;
          *         * the resulting store.
          */
        def step(store: VStore, results: RetVals): StepResult = control match {
            // Evaluate the given expression in the given environment.
            case ControlEval(exp, env) => next(sem.stepEval(exp, env, store, time), store, cc, results)
            // The end of evaluation has been reached. Return the final result.
            case ControlKont(v) if cc == HaltKontAddr => StepResult(Set.empty, Set.empty, Set.empty, Some(v), Set.empty, store)
            // Continue with a given value, given the continuation frame in this state. Pops this frame of the stack.
            case ControlKont(v) =>
                val init: StepResult = StepResult(Set.empty, Set.empty, Set.empty, Option.empty, Set.empty, store)
                kstore.lookup(cc).foldLeft(init)((acc1, konts) => // Lookup all associated continuation frames.
                    konts.foldLeft(acc1){case (acc2, Kont(frame, cc_)) => // For each frame, generate the next actions and accumulate everything (starting from acc1).
                        next(sem.stepKont(v, frame, acc2.store, time), store, cc_, results).merge(acc2) // Note that the frame is popped of the stack by passing cc_.
                })
            // Handle an error. This results in no successor states.
            case ControlError(_) => StepResult(Set.empty, Set.empty, Set.empty, None, Set.empty, store)
            // An unknown control component has been reached. Should not happen so throw an error.
            case e => throw new Exception(s"Unsupported control sequence: $e.\n")
        }
    }
    
    /**
      * Analyses a given program in a thread-modular way. Returns a state graph representing the collecting semantics of the program.
      * @param program The program to be analysed.
      * @param timeout A timeout after which the analysis should stop to prevent looping.
      * @param ev      A implicit graph.
      * @tparam G The type of the graph to be returned.
      * @return Returns a state graph representing the collecting semantics of the program that was analysed.
      */
    def run[G](program: Exp, timeout: Timeout.T)
              (implicit ev: Graph[G, State, Transition]): G = {
        
        type Graphs = Map[TID, Edges]
    
        case class InnerLoopState(created: Created, joined: Joined, effects: Effects, result: V, edges: Edges) {
            def add(crea: Created, joi: Joined, effs: Effects, res: Option[V], edg: Edges): InnerLoopState =  InnerLoopState(
                created ++ crea,
                joined  ++  joi,
                effects ++ effs,
                lattice.join(result, res.getOrElse(lattice.bottom)),
                edges ++ edg
            )
        }
    
        /**
          * Performs the analysis for a single thread.
          * @param work    A worklist.
          * @param results A mapping from TIDs to result values.
          * @param store   The global store.
          * @param visited A list of visited states to be omitted during looping. This argument can (should) be omitted.
          * @param iState  An InnerLoopState containing initial bookkeeping information. This argument can (should) be omitted.
          * @return A set of a new global store and an innerloopstate containing extra bookkeeping information for the outer loop.
          */
        @scala.annotation.tailrec
        def innerLoop(work: List[State], results: RetVals, store: WStore, visited: Set[State] = Set.empty,
                      iState: InnerLoopState = InnerLoopState(Set.empty, Set.empty, Set.empty, lattice.bottom, List.empty)): (WStore, InnerLoopState) = {
            if (timeout.reached || work.isEmpty) (store, iState)
            else {
                val (work_, visited_, store_, iState_): (List[State], Set[State], WStore, InnerLoopState) =
                    work.foldLeft((List.empty[State], visited, store, iState)){case (acc@(workAcc, visitedAcc, storeAcc, iStateAcc), curState) =>
                        if (visitedAcc.contains(curState)) acc // If the state has been explored already, do not take a step.
                        else {                                 // If the state has not been explored yet, take a step.
                            val StepResult(succs, crea, joi, res, effs, sto: WStore) = curState.step(storeAcc, results)
                            val vis = if (sto.updated) Set.empty[State] else visitedAcc + curState // Immediately clear the visited set upon a store change.
                            (workAcc ++ succs, vis, sto.reset, iStateAcc.add(crea, joi, effs, res, succs.toList.map((curState, empty, _))))
                        }
                    }
                innerLoop(work_, results, store_, visited_, iState_)
            }
        }
    
        /**
          * Contains bookkeeping information for the outer loop of the algorithm.
          * @param threads   A map of thread identifiers to sets of initial thread states.
          * @param readDeps  A map indicating the addresses a thread reads.
          * @param writeDeps A map indicating the addresses a thread writes to.
          * @param joinDeps  A map indicating the TIDs a thread joins.
          * @param results   A map of tids to result values.
          * @param store     The store.
          */
        case class OuterLoopState(threads: Threads, readDeps: ReadDeps, writeDeps: WriteDeps, joinDeps: JoinDeps, results: RetVals, store: WStore, graphs: Graphs)
    
        @scala.annotation.tailrec
        def outerLoop(work: List[State], oState: OuterLoopState): Graphs = {
            if (timeout.reached || work.isEmpty) oState.graphs
            else {
                val next: (List[State], OuterLoopState) = work.foldLeft((List[State](), oState)){case ((workAcc, oStateAcc), curState) =>
                    val stid: TID = curState.tid
                    val (store, InnerLoopState(created, joined, effects, result, graph)) = innerLoop(List(curState), oStateAcc.results, oStateAcc.store)
                    // todoCreated contains the initial states of threads that have never been explored. threads is updated accordingly to newThreads to register these new states.
                    val (todoCreated, newThreads): (Set[State], Threads) = created.foldLeft((Set[State](), oStateAcc.threads)) {case ((createdAcc, threadsAcc), curState) =>
                        if (threadsAcc(curState.tid).contains(curState)) (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                        else (createdAcc + curState, threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState)))
                    }
                    // Update module dependencies. todoEffects indicates which modules have to be reanalysed.
                    val joinDeps: JoinDeps = joined.foldLeft(oStateAcc.joinDeps)((depsAcc, curTid) => depsAcc + (curTid -> (depsAcc(curTid) + stid)))
                    val (readDeps, writeDeps): (ReadDeps, WriteDeps) = effects.foldLeft((oStateAcc.readDeps, oStateAcc.writeDeps)){case ((rAcc, wAcc), curEff) => curEff match {
                        case ReadAddrEff(target:  A@unchecked) => (rAcc + (stid -> (rAcc(stid) + target)), wAcc)
                        case WriteAddrEff(target: A@unchecked) => (rAcc, wAcc + (stid -> (wAcc(stid) + target)))
                        case _ => (rAcc, wAcc)
                        }
                    }
                    // Wherever there is an R/W or W/W conflict, add the states that need to be re-explored due to a store change.
                    val todoEffects: List[State] = (readDeps.keySet.foldLeft(Set[TID]())((acc, curTid) =>
                        if (stid != curTid && writeDeps(stid).intersect(oStateAcc.readDeps(curTid)).exists(addr => oStateAcc.store.lookup(addr) != store.lookup(addr)))
                            acc + curTid else acc) ++ writeDeps.keySet.foldLeft(Set[TID]())((acc, curTid) =>
                        if (stid != curTid && writeDeps(stid).intersect(oStateAcc.writeDeps(curTid)).exists(addr => oStateAcc.store.lookup(addr) != store.lookup(addr)))
                            acc + curTid else acc)).toList.flatMap(newThreads)
                    // Join the old and new return value. If the return value changes, all other threads joining in this thread need to be reanalysed.
                    val retVal: V = lattice.join(oStateAcc.results(stid), result)
                    val todoJoined: List[State] = if (oStateAcc.results(stid) == retVal) List.empty else joinDeps(stid).flatMap(newThreads).toList
                    (workAcc ++ todoCreated.toList ++ todoEffects ++ todoJoined,
                     OuterLoopState(newThreads, readDeps, writeDeps, joinDeps, oStateAcc.results + (stid -> retVal), store, oStateAcc.graphs + (stid -> graph)))
                }
                outerLoop(next._1, next._2)
            }
        }
        
        val cc      :          KAddr = HaltKontAddr
        val env     : Environment[A] = Environment.initial[A](sem.initialEnv)
        val control :        Control = ControlEval(program, env)
        val graphs  :         Graphs = Map[TID, Edges]().withDefaultValue(List.empty)
        val kstore  :         KStore = Store.empty[KAddr, Set[Kont]](t)
        val time    :              T = timestamp.initial("")
        val tid     :            TID = allocator.allocate(program, time)
        val state   :          State = State(tid, control, cc, time, kstore)
        val threads :        Threads = Map(tid -> Set(state)).withDefaultValue(Set.empty)
        val vstore  :         VStore = Store.initial[A, V](t, sem.initialStore)(lattice)
        val wstore  :         WStore = WrappedStore[A, V](vstore)(lattice)
        val oState  : OuterLoopState = OuterLoopState(threads,
                                                      Map.empty.withDefaultValue(Set.empty),
                                                      Map.empty.withDefaultValue(Set.empty),
                                                      Map.empty.withDefaultValue(Set.empty),
                                                      Map.empty.withDefaultValue(lattice.bottom),
                                                      wstore, graphs)
        
        outerLoop(List(state), oState).toSet[(TID, Edges)].map(_._2).foldLeft(Graph[G, State, Transition].empty)((acc, curModuleGraph) => acc.addEdges(curModuleGraph))
    }
}

object ConcurrentModular {
    
    /**
      * Wrapper for a store providing change tracking.
      * @param store    The store to be wrapped.
      * @param modified Parameter indicating whether the store has been changed.
      * @param lattice  The lattice corresponding to the value type V.
      * @tparam A Type of addresses, domain of the store.
      * @tparam V Type of values, codomain of the store.
      */
    case class WrappedStore[A <: Address, V](store: Store[A, V], modified: Boolean = false)(implicit val lattice: Lattice[V]) extends Store[A, V] {
        def                       keys: Iterable[A] = store.keys
        def forall(p: ((A, V)) => Boolean): Boolean = store.forall(p)
        def    subsumes(that: Store[A, V]): Boolean = store.subsumes(that)
        def                 lookup(a: A): Option[V] = store.lookup(a)
        def       lookupMF(a: A): MayFail[V, Error] = store.lookupMF(a)
        def         extend(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.extend(a, v))
        def         update(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.update(a, v))
        def updateOrExtend(a: A, v: V): Store[A, V] = verifyChange(a, v, (a, v) => store.updateOrExtend(a, v))
        def    join(that: Store[A, V]): Store[A, V] = verifyMerge(that)
        
        /** Creates a new and updated wrapped store based on this store and an update function for a specific binding.
          * The procedure marks whether the new store differs from the original one for the binding under inspection. */
        private def verifyChange(a: A, v: V, fun: (A, V) => Store[A, V]): Store[A, V] = {
            // Verify whether the store has been changed by comparing the original and new value.
            store.lookup(a) match {
                case None => WrappedStore(fun(a, v), true) // If the entry did not exist yet, the store will change.
                case Some(v1) => // Otherwise, the store only changes when the new and old results differ.
                    val store_ : Store[A, V] = fun(a, v)
                    // Fun must ensure store_ has a mapping for fun's first argument.
                    val v2 : V = store_.lookup(a).getOrElse(throw new Exception(s"Condition failure: missing required binding of $a in store $store_."))
                    WrappedStore(store_, lattice.lteq(v2, v1))
            }
        }
        
        /** Joins two stores and returns an updated wrapped store indicating whether the store has changed. */
        private def verifyMerge(that: Store[A, V]): Store[A, V] = {
            if (store.keys != that.keys)
                WrappedStore(store.join(that), true)
            else {
                // Check subsumption for every element. If at least one element of that is not subsumed by the corresponding element of this,
                // the resulting store will differ from this.
                for (key <- store.keys) {
                    if (!lattice.lteq(that.lookup(key).get, store.lookup(key).get))
                        return WrappedStore(store.join(that), true)
                }
                WrappedStore(store.join(that), false)
            }
        }
        
        /** Get information on whether the store has been updated. */
        def updated: Boolean = modified
        
        /** Reset the updated flag of the store. */
        def reset: WrappedStore[A, V] = WrappedStore(store, false)
    }
}