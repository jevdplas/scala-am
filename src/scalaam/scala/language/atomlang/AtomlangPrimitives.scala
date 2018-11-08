package scalaam.language.atomlang

import scalaam.core.{Address, Error, MayFail, Store}
import scalaam.language.scheme.{SchemeExp, SchemeOps, SchemePrimitives}

/**
  * Trait containing the implementation of the primitives for Atomlang.
  *
  * This trait requires [[scalaam.language.scheme.SchemePrimitives]] to be part of all mix-ins in which it is mixed in,
  * as the Atomlang primitives are a superset of the Scheme primitives.
  *
  * @tparam A The type of addresses.
  * @tparam V The type of values.
  * @tparam T The type of timestamps.
  * @tparam C The type of expressions.
  */
trait AtomlangPrimitives[A <: Address, V, T, C] {
    this: SchemePrimitives[A, V, T, C] =>
    
    /** List of all primitives supported by Atomlang. */
    override def allPrimitives: List[Primitive] = {
        import PrimitiveDefinitions._
        primList ++ List(Atom, Atomp, Deref)
    }
    
    /** Container for the definitions/implementations of Atomlang primitives. */
    object PrimitiveDefinitions {
        
        import PrimitiveDefs._
        import schemeLattice._
        
        /** Lattice operation indicating whether the lattice value represents an atom. */
        def isAtom: V => MayFail[V, Error] = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsAtom)
        
        /** Implementation of the "atom" primitive. */
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
        
        /** Implementation of the "atom?" primitive. */
        object Atomp extends NoStoreOperation("atom?", Some(1)) {
            override def call(v: V) = isAtom(v)
        }
        
        /** Implementation of the "deref" primitive. */
        // TODO: extend this to futures.
        object Deref extends StoreOperation("deref", Some(1)) {
            override def call(v: V, store: Store[A, V]) = {
                for {res <- dereferencePointer(v, store)(deref)} yield (res, store)
            }
        }
        
    }
    
}
