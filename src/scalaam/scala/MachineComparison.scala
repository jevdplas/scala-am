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
import au.com.bytecode.opencsv.CSVWriter
import java.io.BufferedWriter
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

/**  Contains utilities to compare the different machines. Compares both the runtime and state space size. */
object MachineComparison extends App {
    
    type Configuration = (String, MachineAbstraction[SchemeExp, MachineComparison.address.A, MachineComparison.lattice.L, MachineComparison.timestamp.T, SchemeExp] with MachineUtil[SchemeExp, MachineComparison.address.A, MachineComparison.lattice.L])
    type Measurement = (Double, Int) // Runtime, State count
    
    /* **** General configuration **** */
    
    val address   = NameAddress
    val tid       = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    // Need to use type lattice since replaced (int-top) and (bool-top)by constant values (42 and #t).
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S,  Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    
    // Machines
    
    val regAAM = new ConcurrentAAM[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val cncMOD = new ConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    //val botMOD = new ConcurrentModularBottom[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val incMOD = new IncrementalConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val incOPT = new OptimisedIncConcMod[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    
    val configurations: List[Configuration] = List(("regAAM", regAAM), ("cncMOD", cncMOD), ("incMOD", incMOD), ("incOPT", incOPT))
    val timeout: Int = 10 * 60 // 10 minutes
    
    /* **** Experimental setup **** */
    
    val iterations: Int = 10 // todo 30
    val startup:    Int = 3  // todo 10 // Number of iterations to be dropped.
    
    /* **** Experimental output **** */
    
    // Avoid overwriting old results by appending the date and time to the file name.
    val now:                Date =  Calendar.getInstance().getTime
    val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
    val output:           String = "./Results_MachineComparison" + format.format(now) + ".csv"
    val fields:     List[String] = List("Benchmark", "Machine", "States") ++ ((1 to startup).map("s" + _) ++ (1 to iterations).map("i" + _)).toList // Field names for the csv file.
    
    val    out = new BufferedWriter(new FileWriter(output))
    val writer = new CSVWriter(out)
    
    /* **** Benchmarks **** */
    
    object Prelude extends Enumeration {
        type Prelude = Value
        val lock, list, none = Value
    }
    
    import Prelude._
    
    // List of benchmarks with the required prelude (none means only the standard prelude).
    val benchmarks: List[(String, Prelude)] = List(
        
        // Mostly very simple programs (used to test the functioning of the machine).
        
        //("./test/Atomlang/atomicInt.scm",                none),
        //("./test/Atomlang/future-swap.scm",              none),
        //("./test/Atomlang/futurecomplexbody.scm",        none),
        //("./test/Atomlang/list-with-length.scm",         none),
        //("./test/Atomlang/simplefuture.scm",             none),
        //("./test/Atomlang/treiber-stack.scm",            none),
    
        //("./test/Atomlang/Concurrent/simple.scm",        none),
        //("./test/Atomlang/Concurrent/fact2.scm",         none),
        //("./test/Atomlang/Concurrent/fact-indep.scm",    none),
        //("./test/Atomlang/Concurrent/readers2.scm",      none),
        //("./test/Atomlang/Concurrent/lastzero2.scm",     none),
        
        // More complex programs that are more suitable for benchmarking.
        
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
    
    /* **** Experiment implementation **** */
    
    def forFile(file: String, atPrelude: Prelude): Unit = {
        display("\n***** " + file + " *****\n")
        try {
            val f = scala.io.Source.fromFile(file)
            // Add the necessary preludes to the file contents.
            val content: String = StandardPrelude.atomlangPrelude ++ (atPrelude match {
                    case Prelude.lock => lockPrelude
                    case Prelude.list => listPrelude
                    case Prelude.none => ""
                }) ++ f.getLines.mkString("\n")
            f.close()
            val program: SchemeExp = AtomlangParser.parse(content)
            configurations.foreach{ config =>
                try {
                    val result = executeExperiment(program, config) // Measurements for startup are not filtered out!
                    writeStatistics(file, config._1, result)
    
                    val times = result.map(_._1).drop(startup)
                    val meantime: Double = times.sum / Math.max(times.length, 1)
                    val states = result.map(_._2).drop(startup)
                    val meanstat: Double = (states.sum / Math.max(states.length, 1)).toDouble
    
                    display(s"\nTime:\t$meantime\nStates:\t$meanstat\n")
                } catch {
                    case e: Throwable => e.printStackTrace()
                }
            }
        } catch {
            case e: Throwable => e.printStackTrace()
        }
    }
    
    def executeExperiment(program: SchemeExp, configuration: Configuration): List[Measurement] = {
        val machine = configuration._2
        val graph   = DotGraph[machine.State, machine.Transition]()
        
        @scala.annotation.tailrec
        def iterate(n: Int, measurements: List[Measurement]): List[Measurement] = {
            if (n == 0) return measurements.reverse // Restore the order of the measurements.
            System.gc() // Hint towards a gc so it does hopefully not happen during a run.
            val to = Timeout.seconds(timeout) // Start timer.
            val rs = machine.run[graph.G](program, to) // Run benchmark.
            val sc = to.time // Seconds passed.
            val re = sc > timeout // Check whether timeout has occurred.
            val st = rs.nodes
            display(n + " ")
            // If a timeout is reached, this will probably be the case for all iterations, so abort.
            // Also, do not record the result, since it is only partial.
            if (re) return List((-1, st)) // Return the number of states explored before timeout.
            iterate(n - 1, (sc, st) +: measurements)
        }
        display(s"\n${configuration._1} > ")
        iterate(startup + iterations, List())
    }
    
    /* **** Output **** */
    
    // All measurements (also the ones for start-up) are written to the file.
    def writeStatistics(file: String, machine: String, statistics: List[Measurement]): Unit = {
        val name = file.split("/").last
        val num = statistics.size == startup + iterations
        // Make sure every line of the csv is equally long.
        val times = if (num) statistics.map(_._1) else List.fill(startup + iterations)(-1)
        val states = statistics.map(_._2).head // The number of states explored before timeout.
        val line = List(name, machine, states) ++ times
        try {
            writer.writeNext(line.mkString(","))
            writer.flush()
        } catch {
            case e : Throwable => e.printStackTrace()
        }
    }
    
    // Avoid output being buffered.
    def display(data: String): Unit = {
        print(data)
        Console.out.flush()
    }
    
    writer.writeNext(fields.mkString(","))
    writer.flush()
    benchmarks.foreach(Function.tupled(forFile))
    writer.close()
    display("\n\n***** Finished *****\n")
}