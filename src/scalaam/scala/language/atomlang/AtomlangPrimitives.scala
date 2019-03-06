package scalaam.language.atomlang

import scalaam.core.Effects.Effects
import scalaam.core.{Address, Effects, Error, MayFail, Store}
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
        functionalPrimList ++ List(Atom, Atomp, CompareAndSet, Deref, Reset, Futurep)
    }
    
    /** Container for the definitions/implementations of Atomlang primitives. */
    object PrimitiveDefinitions {
        
        import PrimitiveDefs._
        import schemeLattice._
        
        /*
         * Implementation notes
         *
         * Atoms are implemented analogously to cons-cells, i.e. they are represented by means of a pointer
         * pointing into the heap. Upon dereferencing, the pointer is dereferenced. As such, some basic
         * primitives have analogous implementations as follows:         *
         *      atom     ~ cons
         *      deref    ~ car
         *      reset!   ~ set-car!
         */
        
        /** Lattice operation indicating whether the lattice value represents an atom. */
        def isAtom: V => MayFail[V, Error] = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsAtom)
        def isFuture: V => MayFail[V, Error] = schemeLattice.unaryOp(SchemeOps.UnaryOperator.IsFuture)
        
        /** Implementation of the "atom" primitive. */
        object Atom extends Primitive {
            val name = "atom"
            
            // We use a pointer to store the atom. TODO: is this the best way?
            def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): MayFail[(V, Store[A, V], Effects[A]), Error] = args match {
                case (_, v) :: Nil =>
                    val addr = allocator.pointer(fexp, t)
                    MayFail.success((pointer(addr), store.extend(addr, atom(v)), Effects.wAddr(addr)))
                case _ => MayFail.failure(PrimitiveArityError(name, 1, args.size))
            }
        }
        
        /** Implementation of the "atom?" primitive. */
        object Atomp extends NoStoreOperation("atom?", Some(1)) {
            override def call(v: V): MayFail[V, Error] = isAtom(v)
        }
        
        /** Implementation of the "deref" primitive. */
        object Deref extends StoreOperationWithEffs("read", Some(1)) { // Fixme: Change name to deref, but make distinction from futures in semantics. => Need store there.
            override def call(v: V, store: Store[A, V]): MayFail[(V, Store[A, V], Effects[A]), Error] = {
                for {(res, effs) <- dereferencePointerWithEffs(v, store)(deref)} yield (res, store, effs)
            }
        }
    
        /** Implementation of the "compare-and-set!" primitive. */
        // Fixme: should only the comparison & setting be atomic, or also the evaluation of the arguments? => Special form.
        object CompareAndSet extends Primitive {
            val name = "compare-and-set!"
            override def call(fexp: SchemeExp, args: List[(SchemeExp, V)], store: Store[A, V], t: T): MayFail[(V, Store[A, V], Effects[A]), Error] = args match {
                case (_, v) :: (_, old) :: (_, nw) :: Nil =>
                    getPointerAddresses(v).foldLeft(MayFail.success[(V, Store[A, V], Effects[A]), Error]((bottom, store, Effects.noEff())))((acc, addr) =>
                        for {
                            atomv <- store.lookupMF(addr)
                            vatm <- deref(atomv) // TODO
                            (v, store_, effs) <- acc
                            eqv <- Eq.call(old, vatm)
                            res <- ifThenElse(eqv){nw}{vatm}
                            bool <- ifThenElse(eqv){schemeLattice.bool(true)}{schemeLattice.bool(false)}
                        } yield (join(v, bool), store_.update(addr, atom(res)), effs ++ Effects.wAddr(addr) ++ Effects.rAddr(addr)))
                case _ => MayFail.failure(PrimitiveArityError(name, 3, args.length))
            }
        }
        
        /** Implementation of the "reset!" primitive. */
        object Reset extends StoreOperationWithEffs("reset!", Some(2)) {
            override def call(v: V, value: V, store: Store[A, V]): MayFail[(V, Store[A, V], Effects[A]), Error] = {
                // foldLeft: the accumulator is an updated store.
                getPointerAddresses(v).foldLeft(MayFail.success[(Store[A, V], Effects[A]), Error]((store, Effects.noEff())))((acc, addr) =>
                    for {
                        atomv <- store.lookupMF(addr)
                        (store_, effs) <- acc
                        _ <- deref(atomv) // Used for typechecking. TODO: Is this the best way?
                    } yield (store_.update(addr, atom(value)), effs ++ Effects.wAddr(addr))).map{case (store, effs) => (value, store, effs)} // Return value = new value of the atom.
            }
        }
    
        /** Implementation of the "future?" primitive. */
        object Futurep extends NoStoreOperation("future?", Some(1)) {
            override def call(v: V): MayFail[V, Error] = isFuture(v)
        }
    }
    
}
