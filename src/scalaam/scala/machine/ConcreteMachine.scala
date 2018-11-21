package scala.machine

import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.{Graph, NoTransition}
import Graph.GraphOps

import scala.core.MachineUtil

// Based on https://github.com/acieroid/scala-am/blob/e5d16e78418e71cedbc76048013480ac16f5c407/src/main/scala/machine/ConcreteMachine.scala
class ConcreteMachine[Exp, A <: Address, V, T](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    val Action = sem.Action
    
    /** Evaluate an expression using the given semantics and with a given timeout. */
    def eval(exp: Exp, timeout: Timeout.T): ConcreteMachine.this.Result = {
        
        @scala.annotation.tailrec
        def loop(control: Control, store: Store[A, V], stack: List[Frame], t: T): Result = {
            if (timeout.reached) ResultTimeOut()
            else {
                control match {
                    case ControlEval(exp, env) =>
                        val actions = sem.stepEval(exp, env, store, t)
                        if (actions.size == 1) {
                            actions.head match {
                                case Action.Value(v, store) => loop(ControlKont(v), store, stack, Timestamp[T, Exp].tick(t))
                                case Action.Push(f, e, env, store) => loop(ControlEval(e, env), store, f :: stack, Timestamp[T, Exp].tick(t))
                                case Action.Eval(e, env, store) => loop(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t))
                                case Action.StepIn(f, _, e, env, store) => loop(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t, f))
                                case Action.Err(e) => ResultError(e)
                            }
                        }
                        else ResultInvalidState(actions, stack)
                    case ControlKont(v) =>
                        stack match {
                            case Nil => ResultSuccess(v)
                            case f :: cc =>
                                val actions = sem.stepKont(v, f, store, t)
                                if (actions.size == 1) {
                                    actions.head match {
                                        case Action.Value(v, store) => loop(ControlKont(v), store, cc, Timestamp[T, Exp].tick(t))
                                        case Action.Push(f, e, env, store) => loop(ControlEval(e, env), store, f :: cc, Timestamp[T, Exp].tick(t))
                                        case Action.Eval(e, env, store) => loop(ControlEval(e, env), store, cc, Timestamp[T, Exp].tick(t))
                                        case Action.StepIn(f, _, e, env, store) => loop(ControlEval(e, env), store, cc, Timestamp[T, Exp].tick(t, f))
                                        case Action.Err(e) => ResultError(e)
                                    }
                                }
                                else ResultInvalidState(actions, stack)
                        }
                    case ControlError(e) => ResultError(e)
                }
            }
        }
        
        loop(ControlEval(exp, Environment.initial[A](sem.initialEnv)), Store.initial[A, V](t, sem.initialStore), List(), Timestamp[T, Exp].initial(""))
    }
    
    case class State(control: Control, store: Store[A, V], stack: List[Frame], t: T) extends BaseMachineState {
        def halted: Boolean = (stack, control) match {
            case (Nil, ControlKont(_)) => true
            case _ => false
        }
        
        def next(actions: Set[sem.Action.A], store: Store[A, V], stack: List[Frame], t: T): Option[State] = {
            if (actions.size == 1) {
                actions.head match {
                    case Action.Value(v, store) => Some(State(ControlKont(v), store, stack, Timestamp[T, Exp].tick(t)))
                    case Action.Push(f, e, env, store) => Some(State(ControlEval(e, env), store, f :: stack, Timestamp[T, Exp].tick(t)))
                    case Action.Eval(e, env, store) => Some(State(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t)))
                    case Action.StepIn(f, _, e, env, store) => Some(State(ControlEval(e, env), store, stack, Timestamp[T, Exp].tick(t, f)))
                    case Action.Err(e) => Some(State(ControlError(e), store, stack, Timestamp[T, Exp].tick(t)))
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
            case ControlError(_) => None
        }
    }
    
    type Transition = NoTransition
    val empty = new NoTransition
    
    /** Evaluate an expression using the given semantics and with a given timeout. */
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        
        @scala.annotation.tailrec
        def loop(state: State, graph: G): G = {
            if (timeout.reached) graph
            else if (state.halted) graph
            else state.step() match {
                case Some(State(MachineError(_, _, _), _, _, _)) => graph
                case Some(state_) => loop(state_, graph.addEdge(state, empty, state_))
                case None => graph
            }
        }
        
        loop(State(ControlEval(program, Environment.initial[A](sem.initialEnv)), Store.initial[A, V](t, sem.initialStore), List(), Timestamp[T, Exp].initial("")), Graph[G, State, Transition].empty)
    }
}
