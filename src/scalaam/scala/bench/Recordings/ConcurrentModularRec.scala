/*package scala.bench.Recordings

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph._
import scalaam.machine.AAM

class ConcurrentModularRec[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V]) {
    
    import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}

    import scala.machine.ConcurrentModular.WrappedStore
    
    /** Certain parts of this AAM will be reused. */
    val seqAAM = new AAM[Exp, A, V, T](t, sem)
    import seqAAM._
    
    val recorder = new Recorder[TID, A, V]
    import recorder._
    
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
    
    type Edges          = Map[State, Set[(Transition, State)]] // A map is used to overwrite any old edges that would remain present in a List or Set.
    type GraphEdges     = List[(State, Transition, State)]
    type UnlabeledEdges = Map[State, Set[State]]
    
    /** Class used to return all information resulting from stepping this state. */
    case class StepResult(successors: Successors, created: Created, result: Option[V], effects: Effects, store: VStore) {
        // Adds the accumulator. Important: keeps the store of "this".
        def merge(acc: StepResult): StepResult =
            StepResult(successors ++ acc.successors,
                created ++ acc.created,
                Option.empty,
                effects ++ acc.effects,
                store) // Important: keeps the store of "this".
    }
    
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
                              created: Created = Set.empty, result: Option[V] = Option.empty): StepResult = {
            StepResult(Set(successor), created, result, effects, store)
        }
        
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
                recordedCreate.record(tid, ftid)
                val cc_ = KontAddr(fst, time)
                val newPState = State(ftid, ControlEval(fst, env), cc_, timestamp.initial(ftid.toString), Store.empty[KA, Set[Kont]](t).extend(cc_, Set(Kont(frame, HaltKontAddr))))
                val curPState = State(tid, ControlKont(tidv), cc, timestamp.tick(time), kstore)
                StepResult(Set(curPState), Set(newPState), Option.empty, effs ++ Effects.spawn(ftid), store)
            // The semantics wants to read a value from another thread, which needs to be looked up in the 'results' map.
            case DerefFuture(ftid: TID@unchecked, store, effs) =>
                recordedJoin.record(tid, ftid)
                StepResult(results.get(ftid).map(v => State(tid, ControlKont(v), cc, timestamp.tick(time), kstore)).toSet, Set.empty, Option.empty, effs ++ Effects.join(ftid), store)
            // An unknown action has been returned. Should not happen, therefore this is an error.
            case a => throw new Exception(s"Unsupported action: $a.\n")
        }
        
        private def next(actions: Set[Act], old: VStore, cc: KAddr, results: RetVals): StepResult = {
            val init: StepResult = StepResult(Set.empty, Set.empty, Option.empty, Set.empty, old)
            actions.foldLeft(init)((acc, curAction) => act(curAction, time, acc.store, cc, results).merge(acc))
        }
        
        def step(store: VStore, results: RetVals): StepResult = control match {
            // Evaluate the given expression in the given environment.
            case ControlEval(exp, env) => next(sem.stepEval(exp, env, store, time), store, cc, results)
            // The end of evaluation has been reached. Return the final result.
            case ControlKont(v) if cc == HaltKontAddr => StepResult(Set.empty, Set.empty, Some(v), Set.empty, store)
            // Continue with a given value, given the continuation frame in this state. Pops this frame of the stack.
            case ControlKont(v) =>
                val init: StepResult = StepResult(Set.empty, Set.empty, Option.empty, Set.empty, store)
                kstore.lookup(cc).foldLeft(init)((acc1, konts) => // Lookup all associated continuation frames.
                    konts.foldLeft(acc1){case (acc2, Kont(frame, cc_)) => // For each frame, generate the next actions and accumulate everything (starting from acc1).
                        next(sem.stepKont(v, frame, acc2.store, time), store, cc_, results).merge(acc2) // Note that the frame is popped of the stack by passing cc_.
                    })
            // Handle an error. This results in no successor states.
            case ControlError(_) => StepResult(Set.empty, Set.empty, None, Set.empty, store)
            // An unknown control component has been reached. Should not happen so throw an error.
            case e => throw new Exception(s"Unsupported control sequence: $e.\n")
        }
    }
    
    def run[G](program: Exp, timeout: Timeout.T, name: String): (Int, Int, Int) = {
        
        case class InnerLoopState(created: Created, effects: Effects, result: V, edges: UnlabeledEdges) extends SmartHash {
            def add(crea: Created, effs: Effects, res: Option[V], baseState: State, successors: Successors): InnerLoopState =  InnerLoopState(
                created ++ crea,
                effects ++ effs,
                lattice.join(result, res.getOrElse(lattice.bottom)),
                // Replace the edges from a re-evaluated state.
                edges + (baseState -> successors)
            )
        }

        @scala.annotation.tailrec
        def innerLoop(work: List[State], results: RetVals, store: WStore, iteration: Int, visited: Set[State] = Set.empty,
                      iState: InnerLoopState = InnerLoopState(Set.empty, Set.empty, lattice.bottom, Map.empty)): (WStore, InnerLoopState) = {
            if (timeout.reached || work.isEmpty) (store, iState)
            else {
                val (work_, visited_, store_, iState_): (List[State], Set[State], WStore, InnerLoopState) =
                    work.foldLeft((List.empty[State], visited, store, iState)){case (acc@(workAcc, visitedAcc, storeAcc, iStateAcc), curState) =>
                        if (visitedAcc.contains(curState)) acc // If the state has been explored already, do not take a step.
                        else {                                 // If the state has not been explored yet, take a step.
                            val StepResult(succs, crea, res, effs, sto: WStore) = curState.step(storeAcc, results)
                            val vis = if (sto.updated) Set.empty[State] else visitedAcc + curState // Immediately clear the visited set upon a store change.
                            (workAcc ++ succs, vis, sto.reset, iStateAcc.add(crea, effs, res, curState, succs))
                        }
                    }
                innerLoop(work_, results, store_, iteration, visited_, iState_)
            }
        }
        

        case class OuterLoopState(threads: Threads, readDeps: ReadDeps, writeDeps: WriteDeps, joinDeps: JoinDeps, results: RetVals, store: WStore, edges: Edges) extends SmartHash

        @scala.annotation.tailrec
        def outerLoop(work: List[State], oState: OuterLoopState, iteration: Int = 1): OuterLoopState = {
            if (timeout.reached || work.isEmpty) oState
            else {
                val next: (List[State], OuterLoopState) = work.foldLeft((List[State](), oState)){case ((workAcc, oStateAcc), curState) =>
                    val stid: TID = curState.tid
                    val (store, InnerLoopState(created, effects, result, graph)) = innerLoop(List(curState), oStateAcc.results, oStateAcc.store, iteration)
                    // todoCreated contains the initial states of threads that have never been explored. threads is updated accordingly to newThreads to register these new states.
                    val (todoCreated, newThreads): (Set[State], Threads) = created.foldLeft((Set[State](), oStateAcc.threads)) {case ((createdAcc, threadsAcc), curState) =>
                        if (threadsAcc(curState.tid).contains(curState)) (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                        else (createdAcc + curState, threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState)))
                    }
                    // Update module dependencies. todoEffects indicates which modules have to be reanalysed.
                    val (readDeps, writeDeps, joinDeps): (ReadDeps, WriteDeps, JoinDeps) = effects.foldLeft((oStateAcc.readDeps, oStateAcc.writeDeps, oStateAcc.joinDeps)){case ((rAcc, wAcc, jAcc), curEff) => curEff match {
                        case       JoinEff(tid: TID@unchecked) => (rAcc, wAcc, jAcc + (tid -> (jAcc(tid) + stid)))
                        case ReadAddrEff(target:  A@unchecked) =>
                            recordedRead.record(stid, (target, oStateAcc.store.lookup(target).getOrElse(lattice.bottom)))
                            (rAcc + (stid -> (rAcc(stid) + target)), wAcc, jAcc)
                        case WriteAddrEff(target: A@unchecked) =>
                            recordedWrite.record(stid, (target, store.lookup(target).getOrElse(lattice.bottom)))
                            (rAcc, wAcc + (stid -> (wAcc(stid) + target)), jAcc)
                        case _ => (rAcc, wAcc, jAcc)
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
                    (workAcc ++ todoCreated ++ todoEffects ++ todoJoined,
                        OuterLoopState(newThreads, readDeps, writeDeps, joinDeps, oStateAcc.results + (stid -> retVal), store, oStateAcc.edges ++ graph.mapValues(set => set.map((BaseTransition(iteration.toString), _)))))
                }
                outerLoop(next._1, next._2, iteration + 1)
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
        val oState  : OuterLoopState = OuterLoopState(threads,
            Map.empty.withDefaultValue(Set.empty),
            Map.empty.withDefaultValue(Set.empty),
            Map.empty.withDefaultValue(Set.empty),
            Map.empty.withDefaultValue(lattice.bottom),
            wstore,
            Map.empty)
        
        recorder.reset()
        outerLoop(List(state), oState)
        recorder.outputRecorded(name ++ ".modeffs")
        (-1, -1, -1)
    }
}*/
