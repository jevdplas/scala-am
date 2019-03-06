package scalaam.core

object EffectKind extends Enumeration {
    type EffectKind = Value
    val ReadEffect, WriteEffect = Value
}

object EffectTarget extends Enumeration {
    type EffectTarget = Value
    val RefTarget, ThreadTarget = Value
}

import EffectKind._
import EffectTarget._

abstract class Effect[Addr <: Address] {
    val kind: EffectKind
    val tkind: EffectTarget
    val target: Addr
}

trait ReadEffect[Addr <: Address] extends Effect[Addr] {
    val kind: EffectKind = EffectKind.ReadEffect
    
    override def toString: String = s"R:$target"
}

trait WriteEffect[Addr <: Address] extends Effect[Addr] {
    val kind: EffectKind = EffectKind.WriteEffect
    
    override def toString: String = s"W:$target"
}

trait RefEffect[Addr <: Address] extends Effect[Addr] {
    val tkind: EffectTarget = EffectTarget.RefTarget
}

trait ConcEffect[Addr <: Address] extends Effect[Addr] {
    val tkind: EffectTarget = EffectTarget.ThreadTarget
}

case class ReadAddrEff[Addr <: Address](target: Addr) extends ReadEffect[Addr] with RefEffect[Addr]

case class WriteAddrEff[Addr <: Address](target: Addr) extends WriteEffect[Addr] with RefEffect[Addr]

case class SpawnEff[Addr <: Address](target: Addr) extends WriteEffect[Addr] with ConcEffect[Addr]

case class JoinEff[Addr <: Address](target: Addr) extends ReadEffect[Addr] with ConcEffect[Addr]

object Effects {
    type Effects[Addr <: Address] = Set[Effect[Addr]]
    
    def noEff[Addr <: Address](): Effects[Addr] = Set.empty
    
    def rAddr[Addr <: Address](target: Addr): Effects[Addr] = Set(ReadAddrEff(target))
    
    def wAddr[Addr <: Address](target: Addr): Effects[Addr] = Set(WriteAddrEff(target))
    
    def spawn[Addr <: Address](target: Addr): Effects[Addr] = Set(SpawnEff(target))
    
    def  join[Addr <: Address](target: Addr): Effects[Addr] = Set(JoinEff(target))
}