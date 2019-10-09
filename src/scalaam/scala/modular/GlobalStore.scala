package scalaam.modular

import scalaam.core._
import scala.collection.mutable.Map
import scala.collection.mutable.Set

trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] {

  // parameterized by a type that represents (local) addresses
  type LocalAddr <: Address
  // parameterized by a type that represents abstract values
  type Value
  val lattice: Lattice[Value]

  // addresses in the global analysis are (local) addresses of the intra-analysis + the component
  trait Addr extends Address {
    def addr: LocalAddr
    def printable = addr.printable
  }
  case class GlobalAddr(addr: LocalAddr) extends Addr
  case class ComponentAddr(component: IntraComponent, addr: LocalAddr) extends Addr

  // the global store of the analysis
  val store = Map[Addr,Value]().withDefaultValue(lattice.bottom)
  private def updateAddr(addr: Addr, value: Value): Boolean = store.get(addr) match {
    case None if value == lattice.bottom =>
      return false
    case None =>
      store(addr) = value
      return true
    case Some(oldValue) =>
      val newValue = lattice.join(oldValue,value)
      if (newValue == oldValue) { return false }
      store(addr) = newValue
      return true
  }

  // effect that is triggered when an abstract value at address 'addr' is updated
  case class AddrEffect(addr: Addr) extends Effect

  trait GlobalStoreIntra extends super.IntraAnalysis {
    // allocating an address
    def allocAddr(addr: LocalAddr) = ComponentAddr(component,addr)
    // reading addresses if the global store
    protected def readAddr(addr: LocalAddr, component: IntraComponent = component): Value =
      readAddr(ComponentAddr(component,addr))
    protected def readAddr(addr: Addr): Value = {
      pullEffect(AddrEffect(addr))
      store(addr)
    }
    // writing addresses of the global store
    protected def writeAddr(addr: LocalAddr, value: Value, component: IntraComponent = component): Unit =
        writeAddr(ComponentAddr(component,addr),value)
    protected def writeAddr(addr: Addr, value: Value): Unit =
        if (updateAddr(addr,value)) {
          pushEffect(AddrEffect(addr))
        }
    }
}
