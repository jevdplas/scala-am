package scalaam

import scalaam.core._
import scalaam.graph._
import scalaam.language.atomlang._
import scalaam.language.scheme._
import scalaam.lattice._
import scalaam.machine.ConcurrentAAM

import scala.core.MachineUtil
import scala.machine._
import Graph.GraphOps

/**  Contains utilities to compare the different machines. Compares both the runtime and state space size. */
object MachineComparison extends App {
    
    // General configuration
    
    val address   = NameAddress
    val tid       = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S,  Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    
    // Machines
    
    val regAAM = new ConcurrentAAM[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val cncMOD = new ConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val incMOD = new IncrementalConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    
    // Variables for the experiments
    
    type Configuration = (String, MachineAbstraction[SchemeExp, MachineComparison.address.A, MachineComparison.lattice.L, MachineComparison.timestamp.T, SchemeExp] with MachineUtil[SchemeExp, MachineComparison.address.A, MachineComparison.lattice.L])
    type Measurement = (Double, Int) // Runtime, State count
    
    val iterations: Int = 20
    val startup: Int = 3
    val configurations: List[Configuration] = List(("regAAM", regAAM),
                                                   ("cncMOD", cncMOD),
                                                   ("incMOD", incMOD))
    val timeout: Int = 60
    
    // Preludes
    
    object Prelude extends Enumeration {
        type Prelude = Value
        val lock, none = Value
    }
    
    import Prelude._
    
    // List of benchmarks with the required prelude (none means only the standard prelude).
    val benchmarks: List[(String, Prelude)] = List(
        /* Mostly very simple programs (used to test the functioning of the machine).
        ("./test/Atomlang/atomicInt.scm",            none),
        ("./test/Atomlang/cas.scm",                  none),
        ("./test/Atomlang/collector.scm",            none),
        ("./test/Atomlang/future-swap.scm",          none),
        ("./test/Atomlang/futurecomplexbody.scm",    none),
        ("./test/Atomlang/list-with-length.scm",     none),
        ("./test/Atomlang/reset.scm",                none),
        ("./test/Atomlang/simpleatom.scm",           none),
        ("./test/Atomlang/simplefuture.scm",         none),
        ("./test/Atomlang/swap.scm",                 none), */
        ("./test/Atomlang/treiber-stack.scm",        none),
    
        //("./test/Atomlang/Concurrent/simple.scm",    none),
        
        // More complex programs that are more suitable for benchmarking.
        ("./test/Atomlang/Threads/atoms.scm",        none),
        ("./test/Atomlang/Threads/actors.scm",       lock),
        ("./test/Atomlang/Threads/bchain.scm",       lock),
        ("./test/Atomlang/Threads/count.scm",        lock),
        ("./test/Atomlang/Threads/dekker.scm",       none),
        ("./test/Atomlang/Threads/fact.scm",         lock),
        ("./test/Atomlang/Threads/mcarlo.scm",       none),
        ("./test/Atomlang/Threads/mceval.scm",       none),
        ("./test/Atomlang/Threads/minimax.scm",      none),
        ("./test/Atomlang/Threads/msort.scm",        none),
        ("./test/Atomlang/Threads/pc.scm",           lock),
        ("./test/Atomlang/Threads/pp.scm",           lock),
        ("./test/Atomlang/Threads/rng.scm",          lock),
        ("./test/Atomlang/Threads/stm.scm",          lock),
        ("./test/Atomlang/Threads/sudoku.scm",       none),
    )
    
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
    
    // Experiment implementation
    
    def forFile(file: String, atPrelude: Prelude): Unit = {
        println("\n***** " + file + " *****")
        try {
            val f = scala.io.Source.fromFile(file)
            // Add the necessary preludes to the file contents.
            val content: String = StandardPrelude.atomlangPrelude ++ (atPrelude match {
                case Prelude.lock => lockPrelude
                case Prelude.none => ""
            }) ++ f.getLines.mkString("\n")
            f.close()
            val program: SchemeExp = AtomlangParser.parse(content)
            val results = configurations.map(executeExperiment(program, _))
            val statistics = results.map { case (name, measurements) =>
                val times = measurements.map(_._1)
                val meantime = times.sum / times.length
                val stdvtime = Math.sqrt(times.map(t => (t - meantime) * (t - meantime)).sum / times.length)
                val states = measurements.map(_._2)
                val meanstat = (states.sum / states.length).toDouble
                val stdvstat = Math.sqrt(states.map(t => (t - meanstat) * (t - meanstat)).sum / states.length)
                (name, measurements, meantime, stdvtime, meanstat, stdvstat)
            }
            printStatistics(file, statistics)
        } catch {
            case e: Throwable => println(e.getStackTrace)
        }
    }
    
    def executeExperiment(program: SchemeExp, configuration: Configuration): (String, List[Measurement]) = {
        val machine = configuration._2
        val graph   = DotGraph[machine.State, machine.Transition]()
        
        @scala.annotation.tailrec
        def iterate(n: Int, measurements: List[Measurement]): (String, List[Measurement]) = {
            if (n == 0) return (configuration._1, measurements.reverse.drop(startup))
            val t0 = System.nanoTime()
            val rs = machine.run[graph.G](program, Timeout.seconds(timeout))
            val t1 = System.nanoTime()
            val ms = ((t1 - t0) / 1000000).toDouble
            val st = rs.nodes
            print(".")
            iterate(n - 1, (ms, st) +: measurements)
        }
        print(s"\n${configuration._1} > ")
        iterate(startup + iterations, List())
    }
    
    def printStatistics(file: String, statistics: List[(String, List[(Double, Int)], Double, Double, Double, Double)]): Unit = {
        println("\n name   |\tvalue\t|\tmean\t|\tstdev\t|\traw")
        statistics.foreach{s => println(f"${s._1}%s\t   runtime    ${s._3}%09.4f\t  ${s._4}%09.4f\t  ${s._2.map(_._1)}")
                                println(f"           states     ${s._5}%09.4f\t  ${s._6}%09.4f\t  ${s._2.map(_._2)}")}
    }
    
    benchmarks.foreach(Function.tupled(forFile))
}
