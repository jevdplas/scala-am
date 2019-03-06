package scala.machine

import scalaam.core.StoreType.StoreType
import scalaam.core._
//import scalaam.graph.Graph.GraphOps
import scalaam.graph._
//import scalaam.machine.Strategy.Strategy

import scala.core.MachineUtil

class ConcurrentModular[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])(
    implicit val timestamp: Timestamp[T, Exp],
    implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
        with MachineUtil[Exp, A, V] {
    
    def step(): Set[State] = {
        ???
    }
    
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        ???
    }
}
