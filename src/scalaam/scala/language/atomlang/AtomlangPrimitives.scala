package scalaam.language.atomlang

import scalaam.core.{Address, Error, MayFail, Store}
import scalaam.language.scheme.{SchemeExp, SchemeOps, SchemePrimitives}

trait AtomlangPrimitives[A <: Address, V, T, C] {
    this: SchemePrimitives[A, V, T, C] =>
    
    def allPrimitives: List[Primitive] = {
        import PrimitiveDefinitions._
        primList ++ List(Atomp)
    }
    
    object PrimitiveDefinitions {
        
        import PrimitiveDefs._
        import schemeLattice._
        
        def isAtom: V => MayFail[V, Error] = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsAtom)
        
        object Atom extends Primitive {
            val name = "atom"
            
            // We use a pointer to store the atom. TODO: is this the best way?
            def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): MayFail[(V, Store[A, V]), Error] = args match {
                case (_, v) :: Nil => {
                    val addr = allocator.pointer(fexp, t)
                    (pointer(addr), store.extend(addr, atom(v)))
                }
                case _ => MayFail.failure(PrimitiveArityError(name, 1, args.size))
            }
        }
        
        object Atomp extends NoStoreOperation("atom?", Some(1)) {
            override def call(x: V) = isAtom(x)
        }
        
    }
    
}
