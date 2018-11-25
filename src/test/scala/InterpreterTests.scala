import scalaam.core.{Address, _}
import scalaam.language.atomlang.AtomlangSemantics
import scalaam.language.scheme._
import scalaam.machine.AAM

abstract class InterpreterTests[A <: Address, V, T, C](
    kind: BenchmarkTestKind.BenchmarkTestKind)(
    override implicit val timestamp: Timestamp[T, C],
    override implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends Tests[A, V, T, C] with Benchmarks {
    
    import lat._
    import scalaam.language.sexp._
    
    def fromValue(v: Value): V = v match {
        case ValueString(s) => string(s)
        case ValueSymbol(sym) => symbol(sym)
        case ValueInteger(n) => number(n)
        case ValueReal(n) => real(n)
        case ValueBoolean(b) => bool(b)
        case ValueCharacter(c) => char(c)
        case ValueNil => nil
    }

    benchmarksFor(kind).foreach(b =>
        BenchmarksUtil.fileContent(b).foreach(program =>
            property(s"$b should result in ${fromValue(b.result)}") {
                System.gc() /* Runs a GC before, to (maybe) avoid some GC overhead errors */
                checkResult(program, fromValue(b.result), Timeout.seconds(10))
            }
        ))
}

abstract class SchemeInterpreterTests[A <: Address, V, T, C](kind: BenchmarkTestKind.BenchmarkTestKind)(
    override implicit val timestamp: Timestamp[T, C],
    override implicit val lat: SchemeLattice[V, SchemeExp, A]) extends InterpreterTests[A, V, T, C](kind)
    with SchemeBenchmarks

abstract class AtomlangInterpreterTests[A <: Address, V, T, C](kind: BenchmarkTestKind.BenchmarkTestKind)(
    override implicit val timestamp: Timestamp[T, C],
    override implicit val lat: SchemeLattice[V, SchemeExp, A]) extends InterpreterTests[A, V, T, C](kind)
    with AtomlangBenchmarks

class SchemeInterpreterAAMTests[A <: Address, T, V](
    allocator: Allocator[A, T, SchemeExp],
    kind: BenchmarkTestKind.BenchmarkTestKind)(
    implicit val time: Timestamp[T, SchemeExp],
    implicit val l: SchemeLattice[V, SchemeExp, A])
    extends SchemeInterpreterTests[A, V, T, SchemeExp](kind) {
    val sem = new BaseSchemeSemantics[A, V, T, SchemeExp](allocator)
    val machine = new AAM[SchemeExp, A, V, T](StoreType.BasicStore, sem)
}

class AtomlangInterpreterAAMTests[A <: Address, T, V, TID <: ThreadIdentifier](
    allocator: Allocator[A, T, SchemeExp], TIDAllocator: TIDAllocator[TID, T, SchemeExp],
    kind: BenchmarkTestKind.BenchmarkTestKind)(
    implicit val time: Timestamp[T, SchemeExp],
    implicit val l: SchemeLattice[V, SchemeExp, A])
    extends SchemeInterpreterTests[A, V, T, SchemeExp](kind) {
    val sem = new AtomlangSemantics[A, V, T, SchemeExp, TID](allocator, TIDAllocator)
    val machine = new AAM[SchemeExp, A, V, T](StoreType.BasicStore, sem)
}


//class ConcreteSchemeInterpreterAAMTests extends SchemeInterpreterAAMTests[NameAddress.A, ConcreteSchemeTimestamp.T, ConcreteSchemeLattice.L](NameAddress.Alloc[ConcreteSchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunConcrete)
//class ConstantPropagationSchemeInterpreterAAMTests extends SchemeInterpreterAAMTests[NameAddress.A, ZeroCFASchemeTimestamp.T, ConstantPropagationSchemeLattice.L](NameAddress.Alloc[ZeroCFASchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunAbstract)
//class TypeSchemeInterpreterAAMTests extends SchemeInterpreterAAMTests[NameAddress.A, ZeroCFASchemeTimestamp.T, TypeSchemeLattice.L](NameAddress.Alloc[ZeroCFASchemeTimestamp.T, SchemeExp], BenchmarkTestKind.SchemeRunAbstract)

class ConcreteAtomlangInterpreterAAMTests extends AtomlangInterpreterAAMTests[NameAddress.A, ConcreteSchemeTimestamp.T, ConcreteSchemeLattice.L, ConcreteTID.threadID](NameAddress.Alloc[ConcreteSchemeTimestamp.T, SchemeExp], ConcreteTID.Alloc(), BenchmarkTestKind.ConcreteRun)