package scalaam.core

import scala.core.Exp

trait ConcreteVal

object ConcreteVal {
  case class ConcreteNumber(x: Int) extends ConcreteVal
  case class ConcreteReal(x: Double) extends ConcreteVal
  case class ConcreteString(x: String) extends ConcreteVal
  case class ConcreteBool(x: Boolean) extends ConcreteVal
  case class ConcreteChar(x: Char) extends ConcreteVal
  case class ConcreteSymbol(x: String) extends ConcreteVal
  case class ConcretePrim[Primitive](p: Primitive) extends ConcreteVal
  case class ConcreteClosure[E <: Exp, A <: Address](e: E, env: Environment[A]) extends ConcreteVal
  case object ConcreteNil extends ConcreteVal
  case class ConcretePointer[A <: Address](ptr: A) extends ConcreteVal
  case class ConcreteFuture[TID <: ThreadIdentifier](tid: TID) extends ConcreteVal
}
