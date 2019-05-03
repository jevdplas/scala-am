package scala

object BenchConfig {
    
    /* **** Experimental setup **** */
    
    val timeout: Int = 10 * 60 // 10 minutes
    val iterations: Int = 1//10 // todo 30
    val startup:    Int = 0//3  // todo 10 // Number of iterations to be dropped.
    
    /* **** Benchmarks **** */
    
    object Prelude extends Enumeration {
        type Prelude = Value
        val lock, list, none = Value
    }
    
    import Prelude._
    
    // List of benchmarks with the required prelude (none means only the standard prelude).
    val benchmarks: List[(String, Prelude)] = List(
        ("./test/Atomlang/Threads/abp.scm",              lock),
        ("./test/Atomlang/Threads/atoms.scm",            none),
        ("./test/Atomlang/Threads/actors.scm",           lock),
        ("./test/Atomlang/Threads/bchain.scm",           lock),
        ("./test/Atomlang/Threads/count.scm",            lock),
        ("./test/Atomlang/Threads/crypt.scm",            none),
        ("./test/Atomlang/Threads/dekker.scm",           none),
        ("./test/Atomlang/Threads/fact.scm",             lock),
        ("./test/Atomlang/Threads/life.scm",             lock),
        ("./test/Atomlang/Threads/matmul.scm",           none),
        
        ("./test/Atomlang/Threads/mcarlo.scm",           none),
        ("./test/Atomlang/Threads/mceval.scm",           none),
        ("./test/Atomlang/Threads/minimax.scm",          none),
        ("./test/Atomlang/Threads/msort.scm",            none),
        ("./test/Atomlang/Threads/nbody.scm",            none),
        ("./test/Atomlang/Threads/pc.scm",               lock),
        ("./test/Atomlang/Threads/phil.scm",             lock),
        ("./test/Atomlang/Threads/phild.scm",            lock),
        ("./test/Atomlang/Threads/pp.scm",               lock),
        ("./test/Atomlang/Threads/pps.scm",              none),
        
        ("./test/Atomlang/Threads/qsort.scm",            none),
        ("./test/Atomlang/Threads/ringbuf.scm",          lock),
        ("./test/Atomlang/Threads/rng.scm",              lock),
        ("./test/Atomlang/Threads/sieve.scm",            none),
        ("./test/Atomlang/Threads/stm.scm",              lock),
        ("./test/Atomlang/Threads/sudoku.scm",           none),
        ("./test/Atomlang/Threads/trapr.scm",            none),
        ("./test/Atomlang/Threads/tsp.scm",              none),
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
