package scalaam.core

object EffectKind extends Enumeration {
  type EffectKind = Value
  val ReadEffect, WriteEffect = Value
}

object EffectTarget extends Enumeration {
  type EffectTarget = Value
  val StoreTarget, ProcessTarget = Value
}

import EffectKind._
import EffectTarget._

trait EffectKindT {
  val kind: EffectKind
  def prefix: String
}

trait ReadEffectT extends EffectKindT {
  val kind: EffectKind = EffectKind.ReadEffect
  def prefix: String   = "R:"
}

trait WriteEffectT extends EffectKindT {
  val kind: EffectKind = EffectKind.WriteEffect
  def prefix: String   = "W:"
}

trait EffectTargetT {
  val tkind: EffectTarget
  def suffix: String
}

trait StoreEffectT[Addr <: Address] extends EffectTargetT {
  val tkind = EffectTarget.StoreTarget
  val target: Addr
  val suffix: String = target.toString
}

trait ConcEffectT[TID <: ThreadIdentifier] extends EffectTargetT {
  val tkind = EffectTarget.ProcessTarget
  val target: TID
  val suffix: String = target.toString
}

trait Effect {
  this: EffectKindT with EffectTargetT =>
  override def toString: String = prefix + suffix
}

case class ReadAddrEff[Addr <: Address](target: Addr)
    extends Effect
    with StoreEffectT[Addr]
    with ReadEffectT
case class WriteAddrEff[Addr <: Address](target: Addr)
    extends Effect
    with StoreEffectT[Addr]
    with WriteEffectT
case class SpawnEff[TID <: ThreadIdentifier](target: TID)
    extends Effect
    with ConcEffectT[TID]
    with WriteEffectT
case class JoinEff[TID <: ThreadIdentifier](target: TID)
    extends Effect
    with ConcEffectT[TID]
    with ReadEffectT

object Effects {
  type Effects = Set[Effect]
  def noEff(): Effects                                     = Set.empty
  def rAddr[Addr <: Address](target: Addr): Effects        = Set(ReadAddrEff(target))
  def wAddr[Addr <: Address](target: Addr): Effects        = Set(WriteAddrEff(target))
  def spawn[TID <: ThreadIdentifier](target: TID): Effects = Set(SpawnEff(target))
  def join[TID <: ThreadIdentifier](target: TID): Effects  = Set(JoinEff(target))
}
