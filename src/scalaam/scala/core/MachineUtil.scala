package scalaam.core

import scalaam.graph._

trait MachineUtil[Exp <: Expression, A <: Address, V] {
  // This self type allows using sem.Action.A to type actions in this trait. However, it can be removed if necessary, replacing sem.Action.A by _ .
  this: MachineAbstraction[Exp, A, V, _, _] =>

  // Graph utilities.

  type Transition = BaseTransition
  val empty: Transition = BaseTransition()

  // Control components of a machine.

  /** Control component */
  trait Control extends SmartHash

  case class ControlEval(exp: Exp, env: Environment[A]) extends Control {
    override def toString = s"ev($exp[${exp.pos}])"
  }

  case class ControlKont(v: V) extends Control {
    override def toString = s"ko($v)"
  }

  case class ControlError(err: Error) extends Control {
    override def toString = s"err($err)"
  }

  /** Used in the concrete machine to indicate that a wrong number of actions has been received. */
  case class MachineError(err: String, actions: Set[sem.Action.A], stack: List[Frame])
      extends Control {
    override def toString = s"err($err)"
  }

  // Alternative machine output.

  /** Result state of the machine. */
  trait Result extends SmartHash {
    def toString: String

    /** Prints the result to out. */
    def print(): Unit = {
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

  // Machine state.

  /**
    * This trait provides functionalities to generate a state graph from states.<br>
    * <br>
    * Provided functionalities include colouring, labelling and metadata.
    */
  trait BaseMachineState extends GraphElement with SmartHash {
    val control: Control

    def halted: Boolean

    override def toString: String = control.toString

    override def label: String = toString

    override def color: Color =
      if (halted) {
        Colors.Yellow
      } else {
        control match {
          case _: ControlEval  => Colors.Green
          case _: ControlKont  => Colors.Pink
          case _: ControlError => Colors.Red
        }
      }

    override def metadata =
      GraphMetadataMap(
        Map(
          "halted" -> GraphMetadataBool(halted),
          "type" -> (control match {
            case ControlEval(_, _) => GraphMetadataString("eval")
            case ControlKont(_)    => GraphMetadataString("kont")
            case ControlError(_)   => GraphMetadataString("error")
          })
        ) ++ (control match {
          case ControlKont(v) => Map("value" -> GraphMetadataValue[V](v))
          case _              => Map()
        })
      )
  }

}
