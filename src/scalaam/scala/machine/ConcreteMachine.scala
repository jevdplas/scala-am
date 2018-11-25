package scala.machine

import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.Graph
import scalaam.graph.Graph.GraphOps

import scala.core.MachineUtil
import scala.util.control.TailCalls._

// Based on https://github.com/acieroid/scala-am/blob/e5d16e78418e71cedbc76048013480ac16f5c407/src/main/scala/machine/ConcreteMachine.scala
class ConcreteMachine[Exp, A <: Address, V, T](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    import sem.Action.{A => Act, Err, Eval, Push, StepIn, Value}
    
    /** Evaluate an expression using the given semantics and with a given timeout. */
    def eval(exp: Exp, timeout: Timeout.T): ConcreteMachine.this.Result = {
        
        def next(action: Act, store: Store[A, V], stack: List[Frame], t: T): TailRec[Result] = {
            action match {
                case Value(v, store) => tailcall(loop(ControlKont(v), store, stack, Timestamp[T, Exp].tick(t)))
                case Push(f, e, env, store) => tailcall(loop(ControlEval(e, env), store, f :: stack, Timestamp[T, Exp].tick(t)))
                case Eval(e, env, store) => tailcall(loop(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t)))
                case StepIn(f, _, e, env, store) => tailcall(loop(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t, f)))
                case Err(e) => done(ResultError(e))
                case a => throw new Exception(s"Unsupported action: $a.")
            }
        }
        
        def loop(control: Control, store: Store[A, V], stack: List[Frame], t: T): TailRec[Result] = {
            if (timeout.reached) done(ResultTimeOut())
            else {
                control match {
                    case ControlEval(exp, env) =>
                        val actions = sem.stepEval(exp, env, store, t)
                        if (actions.size == 1)
                            tailcall(next(actions.head, store, stack, t))
                        else done(ResultInvalidState(actions, stack))
                    case ControlKont(v) =>
                        stack match {
                            case Nil => done(ResultSuccess(v))
                            case f :: cc =>
                                val actions = sem.stepKont(v, f, store, t)
                                if (actions.size == 1) {
                                    tailcall(next(actions.head, store, cc, t))
                                }
                                else done(ResultInvalidState(actions, stack))
                        }
                    case ControlError(e) => done(ResultError(e))
                }
            }
        }
        
        loop(ControlEval(exp, Environment.initial[A](sem.initialEnv)), Store.initial[A, V](t, sem.initialStore), List(), Timestamp[T, Exp].initial("")).result
    }
    
    case class State(control: Control, store: Store[A, V], stack: List[Frame], t: T) extends BaseMachineState {
        def halted: Boolean = (stack, control) match {
            case (Nil, ControlKont(_)) => true
            case _ => false
        }
        
        def next(actions: Set[sem.Action.A], store: Store[A, V], stack: List[Frame], t: T): Option[State] = {
            if (actions.size == 1) {
                actions.head match {
                    case Value(v, store) => Some(State(ControlKont(v), store, stack, Timestamp[T, Exp].tick(t)))
                    case Push(f, e, env, store) => Some(State(ControlEval(e, env), store, f :: stack, Timestamp[T, Exp].tick(t)))
                    case Eval(e, env, store) => Some(State(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t)))
                    case StepIn(f, _, e, env, store) => Some(State(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t, f)))
                    case Err(e) => Some(State(ControlError(e), store, stack, Timestamp[T, Exp].tick(t)))
                }
            }
            else Some(State(MachineError(s"Expected 1 state, got: ${actions.size}", actions, stack), store, stack, Timestamp[T, Exp].tick(t)))
        }
        
        def step(): Option[State] = control match {
            case ControlEval(exp, env) =>
                val actions = sem.stepEval(exp, env, store, t)
                next(actions, store, stack, t)
            case ControlKont(v) =>
                stack match {
                    case Nil => None
                    case f :: cc =>
                        val actions = sem.stepKont(v, f, store, t)
                        next(actions, store, cc, t)
                }
            case _ => None
        }
    }
    
    /** Evaluate an expression using the given semantics and with a given timeout. */
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        
        @scala.annotation.tailrec
        def loop(state: State, graph: G): G = {
            if (timeout.reached) graph
            else if (state.halted) graph
            else state.step() match {
                case Some(state_) => loop(state_, graph.addEdge(state, empty, state_))
                case None => graph
            }
        }
        
        loop(State(ControlEval(program, Environment.initial[A](sem.initialEnv)),
            Store.initial[A, V](t, sem.initialStore),
            List(),
            Timestamp[T, Exp].initial("")),
            Graph[G, State, Transition].empty)
    }
}