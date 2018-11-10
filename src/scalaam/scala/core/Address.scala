package scalaam.core

/** An address */
trait Address extends SmartHash {
    
    /** Should the address be included when printing an environment or store?
      * This allows to reduce the size of the printed environment/store.
      * Address that are not printable may for example include addresses of primitive functions.
      */
    def printable: Boolean
}

/** An allocator is used to allocate addresses of type A. It relies on
  * timestamps of type T, and contexts of type C */
trait Allocator[A <: Address, T, C] {
    implicit val timestamp: Timestamp[T, C]
    
    /** Allocate a variable given an identifier */
    def variable(name: Identifier, t: T): A
    
    /** Allocate a pointer given some information of type E (usually an expression) */
    def pointer[E](e: E, t: T): A
    
    /** Allocate a primitive */
    def primitive(name: String): A
}

/** The most simple and useful addressing scheme: representing the address of a
  * variable by its name. This completely ignores the timestamp. */
object NameAddress {
    
    trait A extends Address
    
    /** The address of a variable */
    case class Variable(name: Identifier) extends A {
        def printable = true
        
        override def toString = s"@${name.name}"
    }
    
    /** The address for a pointer */
    case class Pointer[E](e: E) extends A {
        def printable = false
    }
    
    /** The address of a primitive */
    case class Primitive(name: String) extends A {
        def printable = false
    }
    
    /** The NameAddress allocator */
    case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends Allocator[A, T, C] {
        def variable(name: Identifier, t: T): A = Variable(name)
        
        def pointer[E](e: E, t: T): A = Pointer(e)
        
        def primitive(name: String) = Primitive(name)
    }
    
}

/** Represent addresses as a combination of names and timestamps. */
object ConcreteAddress {
    
    trait A extends Address
    
    /** Concrete address for a variable. */
    case class Variable[Time](name: Identifier, time: Time) extends A {
        def printable = true
        
        override def toString = s"$name@$time"
    }
    
    /** Concrete address for a pointer. */
    case class Pointer[E, Time](exp: E, time: Time) extends A {
        def printable = false
    }
    
    /** Concrete address for a primitive. */
    case class Primitive(name: String) extends A {
        def printable = false
    }
    
    /** Class providing allocation methods for concrete addresses. */
    case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends Allocator[A, T, C] {
        def variable(name: Identifier, t: T): A = Variable(name, t)
        
        def pointer[E](exp: E, t: T): A = Pointer(exp, t)
        
        def primitive(name: String) = Primitive(name)
    }
    
}