package scalaam.core

import scalaam.graph.{BaseTransition, Graph, GraphElement}

/**
  * The interface of the machine abstraction itself.
  * A machine abstraction is parameterized by:
  *   - A type of expressions Expr
  *   - A type of values V
  *   - A type of timestamp T
  *   - A type of context for the timestamp C
  */
trait MachineAbstraction[Expr, A <: Address, V, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val lattice: Lattice[V]

  /** The semantics used */
  val sem: Semantics[Expr, A, V, T, C]

  /** The states explored by the machine.
   States can be converted to graph nodes */
  type State <: GraphElement

  /** The transitions explored by the machine
      Transitions can be converted to graph edges */
  type Transition <: BaseTransition

  /**
    * Runs the machine abstraction with its semantics on a program.
    *   - @param program is the program to run
    *   - @param graph is the initial graph, on which nodes and edges corresponding respectively to explored states and transitions are added
    *   - @param timeout is a timeout after which the run is stopped
    * When the timeout is exceeded, the run stops and the current graph is returned, but it is not safe to conclude anything from this graph.
    */
  def run[G](program: Expr, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G
}
