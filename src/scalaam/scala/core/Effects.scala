package scalaam.core

trait Effect

case class ReadAddrEff[Addr <: Address](addr: Addr) extends Effect {
  override def toString: String = s"R: $addr"
}
case class WriteAddrEff[Addr <: Address](addr: Addr) extends Effect {
  override def toString: String = s"W: $addr"
}
case class SpawnEff[TID <: ThreadIdentifier](tid: TID) extends Effect {
  override def toString: String = s"S: $tid"
}
case class JoinEff[TID <: ThreadIdentifier](tid: TID) extends Effect {
  override def toString: String = s"J: $tid"
}

object Effects {
  type Effects = Set[Effect]
  def noEff(): Effects                                     = Set.empty
  def rAddr[Addr <: Address](target: Addr): Effects        = Set(ReadAddrEff(target))
  def wAddr[Addr <: Address](target: Addr): Effects        = Set(WriteAddrEff(target))
  def spawn[TID <: ThreadIdentifier](target: TID): Effects = Set(SpawnEff(target))
  def join[TID <: ThreadIdentifier](target: TID): Effects  = Set(JoinEff(target))
}
