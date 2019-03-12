package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.machine.AAM
//import scalaam.graph.Graph.GraphOps
import scalaam.graph._
//import scalaam.machine.Strategy.Strategy

import scala.core.MachineUtil

class ConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}
    
    /** Certain parts of this AAM will be reused. */
    val seqAAM = new AAM[Exp, A, V, T](t, sem)
    
    import seqAAM._
    
    /** Various type declarations. */
    type KAddr      = KA
    type VStore     = Store[A, V]
    type KStore     = Store[KAddr, Set[Kont]]
    
    type Created    = Set[State]
    type Successors = Set[State]
    type Joined     = Set[TID]
    type RetVals    = Map[TID, V]
    
    /**
      * The execution state of one process.
      *
      * @param tid     The thread identifier corresponding to this context.
      * @param control The control component of the thread.
      * @param cc      The address of the current continuation.
      * @param time    A timestamp.
      * @param kstore  A continuation store.
      */
    case class State(tid: TID, control: Control, cc: KAddr, time: T, kstore: KStore) extends GraphElement {
        override def toString: String = control.toString
    
        override def label: String = toString
    
        override def color: Color = if (halted) Colors.Yellow else Colors.Green
    
        override def metadata = GraphMetadataMap(Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("conc")))
        
        /** Indicates whether this state is a final state. */
        def halted: Boolean = (cc, control) match {
            case (HaltKontAddr, ControlKont(_)) => true
            case (_, ControlError(_)) => true
            case _ => false
        }
        
        /** Class used to return all information resulting from stepping this state. */
        case class stepResult(successors: Successors, created: Created, joined: Joined, result: Option[V], effects: Effects, store: VStore) {
            // Adds the accumulator. Important: keeps the store of "this".
            def merge(acc: stepResult): stepResult =
                stepResult(successors ++ acc.successors,
                              created ++ acc.created,
                               joined ++ acc.joined,
                                  Option.empty,
                              effects ++ acc.effects,
                                    store) // Important: keeps the store of "this".
        }
        
        /** Helper function to create new results easily without having to write all fields explicitly. */
        private def newResult(successor: State, effects: Effects, store: VStore,
                              created: Created = Set.empty, joined: Joined = Set.empty, result: Option[V] = Option.empty): stepResult = {
            stepResult(Set(successor), created, joined, result, effects, store)
        }
        
        /** Executes an action on this state and returns the result as well as other bookkeeping information in the form of a stepResults instance. */
        private def act(action: Act, time: T, old: VStore, kstore: KStore, cc: KAddr, results: RetVals): stepResult = action match {
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
                stepResult(Set(curPState), Set(newPState), Set.empty, Option.empty, effs ++ Effects.spawn(ftid), store)
            // The semantics wants to read a value from another thread, which needs to be looked up in the 'results' map.
            case DerefFuture(ftid: TID@unchecked, store, effs) =>
                stepResult(results.get(ftid).map(v => State(tid, ControlKont(v), cc, timestamp.tick(time), kstore)).toSet, Set.empty, Set(ftid), Option.empty, effs ++ Effects.join(ftid), store)
            // An unknown action has been returned. Should not happen, therefore this is an error.
            case a => throw new Exception(s"Unsupported action: $a.\n")
        }
    
        /**
          * Produces the states following this state by applying the given actions successively, thereby updating the store.
          * Returns a stepResult containing of all successor states and bookkeeping information, as well as the final store.
          */
        private def next(actions: Set[Act], old: VStore, cc: KAddr, results: RetVals): stepResult = {
            val init: stepResult = stepResult(Set.empty, Set.empty, Set.empty, Option.empty, Set.empty, old)
            actions.foldLeft(init)((acc, action) => act(action,time, acc.store, kstore, cc, results).merge(acc))
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
        def step(store: VStore, results: RetVals): stepResult = control match {
            // Evaluate the given expression in the given environment.
            case ControlEval(exp, env) => next(sem.stepEval(exp, env, store, time), store, cc, results)
            // The end of evaluation has been reached. Return the final result.
            case ControlKont(v) if cc == HaltKontAddr => stepResult(Set.empty, Set.empty, Set.empty, Some(v), Set.empty, store)
            // Continue with a given value, given the continuation frame in this state. Pops this frame of the stack.
            case ControlKont(v) =>
                val init: stepResult = stepResult(Set.empty, Set.empty, Set.empty, Option.empty, Set.empty, store)
                kstore.lookup(cc).foldLeft(init)((acc1, konts) => // Lookup all associated continuation frames.
                    konts.foldLeft(acc1){case (acc2, Kont(frame, cc_)) => // For each frame, generate the next actions and accumulate everything (starting from acc1).
                        next(sem.stepKont(v, frame, acc2.store, time), store, cc_, results).merge(acc2)
                })
            // Handle an error. This results in no successor states.
            case ControlError(_) => stepResult(Set.empty, Set.empty, Set.empty, None, Set.empty, store)
            // An unknown control component has been reached. Should not happen so throw an error.
            case e => throw new Exception(s"Unsupported control sequence: $e.\n")
        }
    }
    
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = ??? /* {
        case class InnerLoop(work: List[State], visited: Set[State], results: RetVals)
        
        @scala.annotation.tailrec
        def innerLoop(work: List[State], visited: Set[State], store: VStore) = {
            ???
        }
        
        def outerLoop() = ???
    }
    */
}