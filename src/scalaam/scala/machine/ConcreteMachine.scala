package scala.machine

import scalaam.core.StoreType.StoreType
import scalaam.core._

// Based on https://github.com/acieroid/scala-am/blob/e5d16e78418e71cedbc76048013480ac16f5c407/src/main/scala/machine/ConcreteMachine.scala
class ConcreteMachine[Exp, A <: Address, V, T](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V]) {
    
    val Action = sem.Action
    
    /** The control of the machine. */
    trait Control extends SmartHash
    
    case class ControlEval(exp: Exp, env: Environment[A]) extends Control
    
    case class ControlKont(v: V) extends Control
    
    case class ControlError(error: Error) extends Control
    
    /** Result state of the machine. */
    trait Result extends SmartHash {
        def toString: String
        
        /** Prints the result to out. */
        def print() = {
            scala.Predef.print(this.toString)
        }
    }
    
    case class ResultSuccess(v: V) extends Result {
        override def toString: String = "Evaluation reached value: " + v.toString
    }
    
    case class ResultError(e: Error) extends Result {
        override def toString: String = "Evaluation reached error: " + e.toString
    }
    
    case class ResultInvalidState(actions: Set[sem.Action.A], stack: List[Frame]) extends Result {
        override def toString: String =
            s"Evaluation was not concrete. Got ${actions.size} actions instead of 1.\n" +
                "Actions:\n" +
                actions.foldLeft("")((acc, action) => acc + s"\t$action\n") +
                "Stacktrace:\n" +
                stack.take(10).foldLeft("")((acc, frame) => acc + s"\t${frame.toString}\n")
    }
    
    case class ResultTimeOut() extends Result {
        override def toString: String = "Evaluation timed out"
    }
    
    /** Evaluate an expression using the given semantics and with a given timeout. */
    def eval(exp: Exp, timeout: Timeout.T): ConcreteMachine.this.Result = {
        def next(actions: Set[sem.Action.A], store: Store[A, V], stack: List[Frame], t: T): Result = {
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
        }
        
        def loop(control: Control, store: Store[A, V], stack: List[Frame], t: T): Result = {
            if (timeout.reached) ResultTimeOut()
            else {
                control match {
                    case ControlEval(exp, env) =>
                        val actions = sem.stepEval(exp, env, store, t)
                        next(actions, store, stack, t)
                    case ControlKont(v) =>
                        stack match {
                            case Nil => ResultSuccess(v)
                            case f :: cc =>
                                val actions = sem.stepKont(v, f, store, t)
                                next(actions, store, cc, t)
                        }
                    case ControlError(e) => ResultError(e)
                }
            }
        }
        
        loop(ControlEval(exp, Environment.initial[A](sem.initialEnv)), Store.initial[A, V](t, sem.initialStore), List(), Timestamp[T, Exp].initial(""))
    }
}
