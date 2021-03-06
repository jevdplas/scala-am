package scalaam.bench

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

object BenchConfig {

  /* **** Experimental setup **** */

  val timeout: Int    = 20 * 60 // 20 minutes
  val iterations: Int = 30      // ideal: >= 30
  val startup: Int    = 3       // ideal: +- 10 // Number of iterations to be dropped.

  val outputDir: String = "./results_benchmarks/"

  /* **** Benchmarks **** */

  import scalaam.language.LanguagePrelude.Prelude._

  // List of benchmarks with the required prelude (none means only the standard prelude).
  val benchmarks: List[(String, Prelude)] = List(
    /* Unknown time */
    ("./test/Atomlang/VMCAI2020/bubbles.scm", none),

    /* Sub-0.1-second benchmarks */
    ("./test/Atomlang/VMCAI2020/dekker.scm", none), // 0.008
    ("./test/Atomlang/VMCAI2020/rng.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/trapr.scm", none), // 0.01
    ("./test/Atomlang/VMCAI2020/atomicInt.scm", none), // 0.01
    //("./test/Atomlang/VMCAI2020/pp.scm", none), // 0.02
    ("./test/Atomlang/VMCAI2020/pc.scm", none), // 0.02
    //("./test/Atomlang/VMCAI2020/phild.scm", none), // 0.03
    ("./test/Atomlang/VMCAI2020/atoms.scm", none), // 0.05
    ("./test/Atomlang/VMCAI2020/count.scm", none), // 0.05
    ("./test/Atomlang/VMCAI2020/mcarlo.scm", none), // 0.09
    ("./test/Atomlang/VMCAI2020/sieve.scm", none), // 0.01
    //("./test/Atomlang/VMCAI2020/abp.scm", none), // 0.09

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

  // Avoid output being buffered.
  def display(data: String): Unit = {
    print(data)
    Console.out.flush()
  }

  /** Creates a fileName including the given name, suffix and a timestamp. */
  def ts(name: String, suffix: String): String = {
    val now: Date                = Calendar.getInstance().getTime
    val format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm_")
    outputDir + format.format(now) + name + suffix
  }
}
