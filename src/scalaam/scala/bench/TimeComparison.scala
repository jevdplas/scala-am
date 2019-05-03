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

import scalaam.bench.BenchConfig.Prelude
import scalaam.bench.BenchConfig.Prelude.Prelude
import scalaam.bench.BenchConfig._

/**  Contains utilities to compare the different machines. Compares both the runtime and state space size. */
object TimeComparison extends App {
    
    type Configuration = (String, MachineAbstraction[SchemeExp, address.A, lattice.L, timestamp.T, SchemeExp] with MachineUtil[SchemeExp, address.A, lattice.L])
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
    
    val configurations: List[Configuration] = List(/*("regAAM", regAAM),*/ ("cncMOD", cncMOD), /*("incMOD", incMOD),*/ ("incOPT", incOPT))
    val timeout: Int = 10 * 60 // 10 minutes
    
    /* **** Experimental setup **** */
    
    val iterations: Int = 1//10 // todo 30
    val startup:    Int = 0//3  // todo 10 // Number of iterations to be dropped.
    
    /* **** Experimental output **** */
    
    // Avoid overwriting old results by appending the date and time to the file name.
    val now:                Date =  Calendar.getInstance().getTime
    val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
    val output:           String = "./Results_TimeComparison" + format.format(now) + ".csv"
    val fields:     List[String] = List("Benchmark", "Machine", "States") ++ ((1 to startup).map("s" + _) ++ (1 to iterations).map("i" + _)).toList // Field names for the csv file.
    
    val    out = new BufferedWriter(new FileWriter(output))
    val writer = new CSVWriter(out)
    
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
            try {
                System.gc() // Hint towards a gc so it does hopefully not happen during a run.
            } catch {
                // Catch possible "GC overhead limit exceeded" errors due to this forced gc.
                case _: Throwable =>
            }
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
            case e: Throwable => e.printStackTrace()
        }
    }
    
    writer.writeNext(fields.mkString(","))
    writer.flush()
    benchmarks.foreach(Function.tupled(forFile))
    writer.close()
    display("\n\n***** Finished *****\n")
}