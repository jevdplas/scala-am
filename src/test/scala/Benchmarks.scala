import BenchmarkTestKind.BenchmarkTestKind
import org.scalatest._
import scalaam.language.sexp.{ValueBoolean, ValueInteger, ValueString, ValueSymbol}

/** The kind of tests that a benchmark can be used for */
object BenchmarkTestKind extends Enumeration {
    type BenchmarkTestKind = Value
    val SExpParse, SchemeParse = Value
    val ConcreteRun, AbstractRun = Value
    def none: Set[BenchmarkTestKind] = Set.empty
    def all: Set[BenchmarkTestKind] = Set(SExpParse, SchemeParse, ConcreteRun, AbstractRun)
    def parse: Set[BenchmarkTestKind] = Set(SExpParse, SchemeParse)
    def run: Set[BenchmarkTestKind] = Set(ConcreteRun, AbstractRun)
    def except(kinds: Set[BenchmarkTestKind]): Set[BenchmarkTestKind] =
        all -- kinds
}

/** A benchmark
  *
  * @param file      The path to the file.
  * @param result    The expected result for this benchmark as a primitive value.
  * @param supported Supported tests. */
case class Benchmark(file: String, result: scalaam.language.sexp.Value, supported: Set[BenchmarkTestKind.BenchmarkTestKind]) {
    override def toString: String = file
}

object BenchmarksUtil {
    def fileContent(bench: Benchmark): Option[String] = {
        val f = scala.io.Source.fromFile(bench.file)
        val content = f.getLines.mkString("\n")
        f.close()
        Option(content)
    }
}

/** Trait to ease selecting benchmarks for a specific language. */
trait Benchmarks {
    def allBenchmarks: List[Benchmark] = List()
    
    def benchmarksFor(kind: BenchmarkTestKind): List[Benchmark] =
        allBenchmarks.filter(b => b.supported.contains(kind))
    
    val unused: List[Benchmark] = allBenchmarks.filter(b => b.supported.isEmpty)
}

trait SchemeBenchmarks extends Benchmarks {
    override def allBenchmarks: List[Benchmark] = super.allBenchmarks ++ Benchmarks.schemeBenchmarks
}

trait AtomlangBenchmarks extends Benchmarks {
    override def allBenchmarks: List[Benchmark] = super.allBenchmarks ++ Benchmarks.atomlangBenchmarks
}
// [merge] disable this
// =======
//   def fileContent(bench: Benchmark): Option[String] = {
//     val f = scala.io.Source.fromFile(bench.file)(scala.io.Codec("UTF-8"))
//     val content = f.getLines.mkString("\n")
//     f.close()
//     Option(content)
//   }
// }
// 
// object Benchmarks {
//   import BenchmarkTestKind._
//   import scalaam.language.sexp._
//   def benchmarksFor(kind: BenchmarkTestKind): List[Benchmark] =
//     allBenchmarks.filter(b => b.supported.contains(kind))
//   def allBenchmarks = List(
//     Benchmark("test/ad/abstrct.scm", ValueBoolean(true), parse), // strange equal bug, which makes it so that it doesn't terminate with GAAM
//     Benchmark("test/ad/bfirst.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/ad/bst.scm", ValueBoolean(true), none), // dot notation
//     // TODO: ad/btree.scm only contains definition, add a body
//     Benchmark("test/ad/bubsort.scm", ValueBoolean(true), none), // fails to parse
//     Benchmark("test/ad/dict.scm", ValueBoolean(true), all),
//     Benchmark("test/ad/heap.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/ad/inssort.scm", ValueBoolean(true), parse), // not concrete execution
//     Benchmark("test/ad/linear.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/ad/list.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/ad/mesort.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/ad/prioq.scm", ValueBoolean(true), all),
//     Benchmark("test/ad/qsort.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/ad/qstand.scm", ValueBoolean(true), parse /* equal? not correct for vectors */),
//     Benchmark("test/ad/queue.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/ad/quick.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/ad/RBtreeADT.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/ad/selsort.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/ad/stack.scm", ValueBoolean(true), all),
//     Benchmark("test/ad/stspaceCODE.scm", ValueBoolean(true), none), // dot notation
// 
//     Benchmark("test/blur.scm", ValueBoolean(true), all),
//     Benchmark("test/bound-precision.scm", ValueBoolean(true), all),
//     Benchmark("test/church-2-num.scm", ValueInteger(2), all),
//     Benchmark("test/church-6.scm", ValueInteger(6), all),
//     Benchmark("test/church.scm", ValueBoolean(true), all),
//     Benchmark("test/collatz.scm", ValueInteger(5), all),
//     Benchmark("test/count.scm", ValueString("done"), all),
//     Benchmark("test/eta.scm", ValueBoolean(false), all),
//     Benchmark("test/fact.scm", ValueInteger(120), all),
//     Benchmark("test/fib.scm", ValueInteger(3), all),
// 
// Benchmark("test/gabriel/boyer.scm", ValueBoolean(true), all),
//     Benchmark("test/gabriel/cpstak.scm", ValueInteger(6), all),
//     Benchmark("test/gabriel/dderiv.scm", ValueBoolean(true), all),
//     Benchmark("test/gabriel/deriv.scm", ValueBoolean(true), all),
//     Benchmark("test/gabriel/divrec.scm", ValueBoolean(true), all),
//     Benchmark("test/gabriel/takl.scm", ValueBoolean(true), all),
// 
//     Benchmark("test/gambit/array1.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/browse.scm", ValueInteger(1101), parse /* missing string-ref */),
//     Benchmark("test/gambit/cat.scm", ValueBoolean(true), parse /* rely on IO */),
//     Benchmark("test/gambit/compiler.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/gambit/ctak.scm", ValueBoolean(true), parse /* call/cc */),
//     Benchmark("test/gambit/deriv.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/destruc.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/diviter.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/earley.scm", ValueBoolean(true), parse /* list->vector */),
//     Benchmark("test/gambit/fibc.scm", ValueBoolean(true), parse /* call/cc */),
//     Benchmark("test/gambit/graphs.scm", ValueBoolean(true), except(SchemeRunConcrete) /* loses precision in concrete */),
//     Benchmark("test/gambit/lattice.scm", ValueBoolean(true), parse /* apply */),
//     Benchmark("test/gambit/matrix.scm", ValueBoolean(true), except(SchemeRunConcrete) /* loses precision in concrete */),
//     Benchmark("test/gambit/mazefun.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/nboyer.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/gambit/nqueens.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/paraffins.scm", ValueBoolean(true), except(SchemeRunConcrete) /* incorrect concrete results, probably due to integer overflow */),
//     Benchmark("test/gambit/perm9.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/peval.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/primes.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/puzzle.scm", ValueBoolean(true), parse /* call/cc */),
//     Benchmark("test/gambit/sboyer.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/gambit/scheme.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/gambit/slatex.scm", ValueBoolean(true), none), // dot notation
//     Benchmark("test/gambit/string.scm", ValueBoolean(true), parse /* missing substring */),
//     Benchmark("test/gambit/sum.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/sumloop.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/tail.scm", ValueBoolean(true), parse /* rely on IO */),
//     Benchmark("test/gambit/tak.scm", ValueBoolean(true), all),
//     Benchmark("test/gambit/trav1.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/gambit/triangl.scm", ValueBoolean(true), parse /* list->vector */),
//     Benchmark("test/gambit/wc.scm", ValueBoolean(true), parse /* rely on IO */),
// 
//     Benchmark("test/gcipd.scm", ValueInteger(36), all),
//     Benchmark("test/grid.scm", ValueBoolean(true), all),
//     Benchmark("test/inc.scm", ValueInteger(4), all),
// /* TODO[easy] these should return bottom
//     Benchmark("test/infinite-1.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
//     Benchmark("test/infinite-2.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
//     Benchmark("test/infinite-3.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
//  */
//     Benchmark("test/kcfa2.scm", ValueBoolean(false), all),
//     Benchmark("test/kcfa3.scm", ValueBoolean(false), all),
//     Benchmark("test/kernighanvanwyk/ack.scm", ValueInteger(4), all),
//     Benchmark("test/letrec-begin.scm", ValueInteger(1), all),
//     Benchmark("test/loop2.scm", ValueInteger(550), all),
//     Benchmark("test/mceval.scm", ValueInteger(40320), all),
//     Benchmark("test/mj09.scm", ValueInteger(2), all),
//     Benchmark("test/mut-rec.scm", ValueBoolean(true), all),
//     Benchmark("test/nested-defines.scm", ValueBoolean(true), all),
//     Benchmark("test/primtest.scm", ValueInteger(1), all),
//     Benchmark("test/quasiquoting-simple.scm", ValueBoolean(true), none), // quasiquoting
//     Benchmark("test/quasiquoting.scm", ValueBoolean(true), none), // quasiquoting
//     Benchmark("test/regex.scm", ValueBoolean(true), all),
//     Benchmark("test/rosetta/easter.scm", ValueBoolean(true), all),
//     Benchmark("test/rosetta/quadratic.scm", ValueBoolean(true), all),
//     Benchmark("test/rotate.scm", ValueString("hallo"), all),
//     Benchmark("test/rsa.scm", ValueBoolean(true), all),
//     Benchmark("test/sat.scm", ValueBoolean(true), all),
//     Benchmark("test/scm2c.scm", ValueBoolean(true), none), // unknown reason
//     Benchmark("test/scm2java.scm", ValueString("public class BOut extends RuntimeEnvironment {\\n public static void main (String[] args) {\\nnew IntValue(3) ;\\n }\\n}\\n"), all),
//     Benchmark("test/scp1/2.1.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/2.4.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/3.1.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/3.2.1.scm", ValueBoolean(true), all),
//     Benchmark("test/scp1/3.2.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/3.3.scm",   ValueBoolean(true), except(SchemeRunConcrete)), /* no bigint support, necessary to get the correct result */
//     Benchmark("test/scp1/3.4.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/3.6.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/3.8.scm",   ValueBoolean(true), except(SchemeRunConcrete) /* bigint needed? */),
//     Benchmark("test/scp1/3.9.scm",   ValueBoolean(true), parse /* takes too long? */),
//     Benchmark("test/scp1/4.1.scm",   ValueBoolean(true), parse /* takes too long? */),
//     Benchmark("test/scp1/4.8.scm",   ValueBoolean(true), except(SchemeRunConcrete) /* bigint needed? */),
//     Benchmark("test/scp1/5.14.3.scm",ValueBoolean(true), all),
//     Benchmark("test/scp1/5.19.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/5.20.4.scm",ValueBoolean(true), all),
//     Benchmark("test/scp1/5.21.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/5.22.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/5.6.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/5.7.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/7.11.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.12.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.13.scm",  ValueBoolean(true), parse), // equal? bug
//     Benchmark("test/scp1/7.14.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.15.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.16.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.17.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/7.18.scm",  ValueBoolean(true), parse /* unknown */),
//     Benchmark("test/scp1/7.2.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/7.3.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/7.4.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/7.5.scm",   ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/7.6.scm",   ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/7.9.scm",   ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/8.1.1.scm", ValueBoolean(true), all),
//     Benchmark("test/scp1/8.1.3.scm", ValueBoolean(true), all),
//     Benchmark("test/scp1/8.10.scm",  ValueBoolean(true), parse), // equal bug
//     Benchmark("test/scp1/8.11.scm",  ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/8.12.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/8.13.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/8.14.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/8.15.scm",  ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/8.16.scm",  ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/8.5.scm",   ValueBoolean(true), none), // dot notation
//     Benchmark("test/scp1/8.6.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/9.12.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.13.scm",  ValueBoolean(true), parse /* unknown */),
//     Benchmark("test/scp1/9.14.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.15.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.16.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.17.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.18.scm",  ValueBoolean(true), all),
//     Benchmark("test/scp1/9.2.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/9.3.scm",   ValueBoolean(true), all),
//     Benchmark("test/scp1/9.5.scm",   ValueBoolean(true), parse /* unknown */),
//     Benchmark("test/scp1/9.6.scm",   ValueBoolean(true), except(SchemeRunConcrete) /* bug/imprecision that seems related to equality on pairs */),
//     Benchmark("test/scp1/9.7.scm",   ValueBoolean(true), parse /* unknown */),
//     Benchmark("test/scp1/9.8.scm",   ValueBoolean(true), parse /* unknown */),
//     Benchmark("test/scp1/9.9.scm",   ValueBoolean(true), except(SchemeRunConcrete) /* similar to 9.6.scm */),
// >>>>>>> 92fa163013b9a2379a3d93c2ed499193f90ef037

trait AllBenchmarks extends SchemeBenchmarks with AtomlangBenchmarks

class UnusedBenchmarksTests extends FlatSpec with Matchers with AllBenchmarks {
    unused.foreach(bench => bench.file should "be used" in {
        cancel("unused")
    })
}

object Benchmarks {
    
    import BenchmarkTestKind._
    
    val schemeBenchmarks = List(
        Benchmark("test/Scheme/ad/abstrct.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/bfirst.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/ad/bst.scm", ValueBoolean(true), none), // dot notation
        // TODO: ad/btree.scm only contains definition, add a body
        Benchmark("test/Scheme/ad/bubsort.scm", ValueBoolean(true), none), // fails to parse
        Benchmark("test/Scheme/ad/dict.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/heap.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/ad/inssort.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/linear.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/ad/list.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/ad/mesort.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/ad/prioq.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/qsort.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/ad/qstand.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/queue.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/ad/quick.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/ad/RBtreeADT.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/ad/selsort.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/ad/stack.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/ad/stspaceCODE.scm", ValueBoolean(true), none), // dot notation
        
        Benchmark("test/Scheme/blur.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/bound-precision.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/church-2-num.scm", ValueInteger(2), all),
        Benchmark("test/Scheme/church-6.scm", ValueInteger(6), all),
        Benchmark("test/Scheme/church.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/collatz.scm", ValueInteger(5), all),
        Benchmark("test/Scheme/count.scm", ValueString("done"), all),
        Benchmark("test/Scheme/eta.scm", ValueBoolean(false), all),
        Benchmark("test/Scheme/fact.scm", ValueInteger(120), all),
        Benchmark("test/Scheme/fib.scm", ValueInteger(3), all),
        
        Benchmark("test/Scheme/gabriel/boyer.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gabriel/cpstak.scm", ValueInteger(6), all),
        Benchmark("test/Scheme/gabriel/dderiv.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gabriel/deriv.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gabriel/divrec.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gabriel/takl.scm", ValueBoolean(true), all),
        
        Benchmark("test/Scheme/gambit/array1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/browse.scm", ValueInteger(1101), parse /* missing string-ref */),
        Benchmark("test/Scheme/gambit/cat.scm", ValueBoolean(true), parse /* rely on IO */),
        Benchmark("test/Scheme/gambit/compiler.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/gambit/ctak.scm", ValueBoolean(true), parse /* call/cc */),
        Benchmark("test/Scheme/gambit/deriv.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/destruc.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/diviter.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/earley.scm", ValueBoolean(true), parse /* list->vector */),
        Benchmark("test/Scheme/gambit/fibc.scm", ValueBoolean(true), parse /* call/cc */),
        Benchmark("test/Scheme/gambit/graphs.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/lattice.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/matrix.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/mazefun.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/nboyer.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/gambit/nqueens.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/paraffins.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/perm9.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/peval.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/primes.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/puzzle.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/sboyer.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/gambit/scheme.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/gambit/slatex.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/gambit/string.scm", ValueBoolean(true), parse /* missing substring */),
        Benchmark("test/Scheme/gambit/sum.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/sumloop.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/tail.scm", ValueBoolean(true), parse /* rely on IO */),
        Benchmark("test/Scheme/gambit/tak.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/gambit/trav1.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/gambit/triangl.scm", ValueBoolean(true), parse /* list->vector */),
        Benchmark("test/Scheme/gambit/wc.scm", ValueBoolean(true), parse /* rely on IO */),
        
        Benchmark("test/Scheme/gcipd.scm", ValueInteger(36), all),
        Benchmark("test/Scheme/grid.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/inc.scm", ValueInteger(4), all),
        /* TODO[easy] these should return bottom
            Benchmark("test/Scheme/infinite-1.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
            Benchmark("test/Scheme/infinite-2.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
            Benchmark("test/Scheme/infinite-3.scm", all - SchemeRunConcrete /* does not terminate in concrete (expected behavior) */),
         */
        Benchmark("test/Scheme/kcfa2.scm", ValueBoolean(false), all),
        Benchmark("test/Scheme/kcfa3.scm", ValueBoolean(false), all),
        Benchmark("test/Scheme/kernighanvanwyk/ack.scm", ValueInteger(4), all),
        Benchmark("test/Scheme/letrec-begin.scm", ValueInteger(1), all),
        Benchmark("test/Scheme/loop2.scm", ValueInteger(550), all),
        Benchmark("test/Scheme/mceval.scm", ValueInteger(40320), all),
        Benchmark("test/Scheme/mj09.scm", ValueInteger(2), all),
        Benchmark("test/Scheme/mut-rec.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/nested-defines.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/primtest.scm", ValueInteger(1), all),
        Benchmark("test/Scheme/quasiquoting-simple.scm", ValueBoolean(true), none), // quasiquoting
        Benchmark("test/Scheme/quasiquoting.scm", ValueBoolean(true), none), // quasiquoting
        Benchmark("test/Scheme/regex.scm", ValueBoolean(false), all),
        Benchmark("test/Scheme/rosetta/easter.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/rosetta/quadratic.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/rotate.scm", ValueString("hallo"), all),
        Benchmark("test/Scheme/rsa.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/sat.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scm2c.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/scm2java.scm", ValueString("public class BOut extends RuntimeEnvironment {\\n public static void main (String[] args) {\\nnew IntValue(3) ;\\n }\\n}\\n"), all),
        Benchmark("test/Scheme/scp1/2.1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/2.4.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.2.1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.2.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.3.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.4.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.6.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.8.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/3.9.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/4.1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/4.8.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.14.3.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.19.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.20.4.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.21.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.22.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.6.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/5.7.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.11.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.12.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.13.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.14.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.15.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.16.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.17.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.18.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.2.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.3.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.4.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/7.5.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/7.6.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/7.9.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/8.1.1.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.1.3.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.10.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.11.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/8.12.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.13.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.14.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/8.15.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/8.16.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/8.5.scm", ValueBoolean(true), none), // dot notation
        Benchmark("test/Scheme/scp1/8.6.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.12.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.13.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.14.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.15.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.16.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.17.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.18.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.2.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.3.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.5.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.6.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.7.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.8.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/scp1/9.9.scm", ValueBoolean(true), all),
        
        Benchmark("test/Scheme/SICP-compiler.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/sigscheme/arithint.scm", ValueInteger(20001), all),
        Benchmark("test/Scheme/sigscheme/case.scm", ValueInteger(20000), all),
        Benchmark("test/Scheme/sigscheme/let-loop.scm", ValueInteger(20000), all),
        Benchmark("test/Scheme/sigscheme/loop.scm", ValueInteger(8000), all),
        Benchmark("test/Scheme/sigscheme/mem.scm", ValueBoolean(false), all),
        Benchmark("test/Scheme/sigscheme/rec.scm", ValueBoolean(true), all),
        Benchmark("test/Scheme/sigscheme/takr.scm", ValueInteger(7), all),
        Benchmark("test/Scheme/sq.scm", ValueInteger(9), all),
        Benchmark("test/Scheme/Streams.scm", ValueBoolean(true), none), // unknown reason
        Benchmark("test/Scheme/sym.scm", ValueSymbol("foo"), all),
        Benchmark("test/Scheme/widen.scm", ValueInteger(10), all),
        Benchmark("test/Scheme/work.scm", ValueInteger(362880), all)
    )
    
    val atomlangBenchmarks = List(
        Benchmark("test/Atomlang/Concurrent/count2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count3.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count4.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count5.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count6.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count7.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count8.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count9.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count10.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count11.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count12.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count13.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count14.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/count15.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/fact2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/fact-indep.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/indexer2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer3.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer4.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer5.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer6.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer7.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer8.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer9.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer10.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer11.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer12.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer13.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer14.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/indexer15.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/lastzero2.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/mutex2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/mutex3.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/mutex4.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/mutex5.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/mutex6.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/pcounter2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter3.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter4.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter5.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter6.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter7.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter8.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter9.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter10.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter11.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter12.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter13.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter14.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/pcounter15.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/race2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/race3.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/race4.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/race5.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/race6.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/Concurrent/readers2.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/Concurrent/simple.scm", ValueBoolean(true), none),
        
        Benchmark("test/Atomlang/atomicInt.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/cas.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/collector.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/future-swap.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/list-with-length.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/reset.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/simpleatom.scm", ValueBoolean(true), all),
        Benchmark("test/Atomlang/swap.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/validator.scm", ValueBoolean(true), none),
        Benchmark("test/Atomlang/validator-swap-multiarg.scm", ValueBoolean(true), none)
    )
}

