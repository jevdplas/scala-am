package scalaam.bench

object BenchConfig {

  /* **** Experimental setup **** */

  val timeout: Int    = 20 * 60 // 20 minutes
  val iterations: Int = 1      // ideal: >= 30
  val startup: Int    = 0       // ideal: +- 10 // Number of iterations to be dropped.

  val outputDir: String = "./results_benchmarks/"

  /* **** Benchmarks **** */

  object Prelude extends Enumeration {
    type Prelude = Value
    val lock, list, none = Value
  }

  import Prelude._

  // List of benchmarks with the required prelude (none means only the standard prelude).
  val benchmarks: List[(String, Prelude)] = List(
    /* Unknown time */
    ("./test/Atomlang/VMCAI2020/bubbles.scm", none),
    ("./test/Atomlang/VMCAI2020/msort2.scm", none),

    /* Sub-0.1-second benchmarks */
    ("./test/Atomlang/VMCAI2020/dekker.scm", none), // 0.008
    ("./test/Atomlang/VMCAI2020/rng.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/trapr.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/atomicInt.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/pp.scm", none), // 0.02
    ("./test/Atomlang/VMCAI2020/pc.scm", none), // 0.02
    ("./test/Atomlang/VMCAI2020/phild.scm", none), // 0.03
    ("./test/Atomlang/VMCAI2020/atoms.scm", none), // 0.05
    ("./test/Atomlang/VMCAI2020/count.scm", none), // 0.05
    ("./test/Atomlang/VMCAI2020/mcarlo.scm", none), // 0.09
    ("./test/Atomlang/VMCAI2020/sieve.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/abp.scm", none), // 0.09

    /* 0.1 - 1 seconds */
    ("./test/Atomlang/VMCAI2020/treiber-stack.scm", none), // 0.1
    ("./test/Atomlang/VMCAI2020/msort2.scm", none), // 0.1
    ("./test/Atomlang/VMCAI2020/msort.scm", none), // 0.4
    ("./test/Atomlang/VMCAI2020/sudoku-dynamic.scm", none), // 0.2
    ("./test/Atomlang/VMCAI2020/fact.scm", none), // 0.25
    ("./test/Atomlang/VMCAI2020/mqsort.scm", none), // 0.4
    ("./test/Atomlang/VMCAI2020/bchain.scm", none), // 0.4
    ("./test/Atomlang/VMCAI2020/actors.scm", none), // 0.4

    /* 1-2s benchmarks */
    ("./test/Atomlang/VMCAI2020/sudoku.scm", none), // 1.1
    ("./test/Atomlang/VMCAI2020/life.scm", none), // 1.2
    ("./test/Atomlang/VMCAI2020/matmul.scm", none), // 3.5
    ("./test/Atomlang/VMCAI2020/minimax-less-futures.scm", none), // 5.1

    /* > 10s benchmarks */
    ("./test/Atomlang/VMCAI2020/minimax.scm", none), // 31.5
    ("./test/Atomlang/VMCAI2020/mceval.scm", none), // 105
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
