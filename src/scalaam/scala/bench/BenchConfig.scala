package scalaam.bench

object BenchConfig {

  /* **** Experimental setup **** */

  val timeout: Int    = 20 * 60 // 20 minutes
  val iterations: Int = 20      // ideal: >= 30
  val startup: Int    = 3       // ideal: +- 10 // Number of iterations to be dropped.

  /* **** Benchmarks **** */

  object Prelude extends Enumeration {
    type Prelude = Value
    val lock, list, none = Value
  }

  import Prelude._

  // List of benchmarks with the required prelude (none means only the standard prelude).
  val benchmarks: List[(String, Prelude)] = List(
    ("./test/Atomlang/VMCAI2020/abp.scm", none),
    ("./test/Atomlang/VMCAI2020/actors.scm", none),
    ("./test/Atomlang/VMCAI2020/atomicInt.scm", none),
    ("./test/Atomlang/VMCAI2020/atoms.scm", none),
 // ("./test/Atomlang/VMCAI2020/bchain.scm", none), // too slow to generate graph
    ("./test/Atomlang/VMCAI2020/count.scm", none),
    ("./test/Atomlang/VMCAI2020/dekker.scm", none),
    ("./test/Atomlang/VMCAI2020/fact.scm", none),
    ("./test/Atomlang/VMCAI2020/life.scm", none),
    ("./test/Atomlang/VMCAI2020/matmul.scm", none),
    ("./test/Atomlang/VMCAI2020/mcarlo.scm", none),
    ("./test/Atomlang/VMCAI2020/mceval.scm", none),
    ("./test/Atomlang/VMCAI2020/msort.scm", none),
    ("./test/Atomlang/VMCAI2020/pc.scm", none),
 // ("./test/Atomlang/VMCAI2020/phild.scm", none), // deadlocks in some cases
 // ("./test/Atomlang/VMCAI2020/pp.scm", none), // too slow to generate graph
    ("./test/Atomlang/VMCAI2020/rng.scm", none),
    ("./test/Atomlang/VMCAI2020/sieve.scm", none),
    ("./test/Atomlang/VMCAI2020/sudoku.scm", none),
    ("./test/Atomlang/VMCAI2020/trapr.scm", none),
    ("./test/Atomlang/VMCAI2020/treiber-stack.scm", none),
    ("./test/Atomlang/VMCAI2020/minimax.scm", none) // Sbt tends to hang here on Bertha.
    )

  // Lock implementation by means of atoms.
  val lockPrelude: String =
    """(define (t/new-lock)
          |  (atom #f))
          |(define (t/acquire lock)
          |  (let try ()
          |    (if (compare-and-set! lock #f #t)
          |        #t
          |        (try))))
          |(define (t/release lock)
          |  (reset! lock #f))""".stripMargin

  // Implementation of two basic list primitives.
  val listPrelude: String =
    """(define (map f l)
          |  (if (null? l)
          |      '()
          |      (cons (f (car l))
          |            (map f (cdr l)))))
          |(define (for-each f l)
          |  (if (not (null? l))
          |      (begin (f (car l))
          |             (for-each f (cdr l)))))""".stripMargin

  // Avoid output being buffered.
  def display(data: String): Unit = {
    print(data)
    Console.out.flush()
  }
}
