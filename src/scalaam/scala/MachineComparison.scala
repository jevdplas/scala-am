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
    
    val iterations: Int = 15
    val configurations: List[Configuration] = List(("regAAM", regAAM),
                                                   ("cncMOD", cncMOD),
                                                   ("incMOD", incMOD))
    val timeout: Timeout.T = Timeout.seconds(60)
    
    val benchmarks: List[String] = List("./test/Atomlang/Concurrent/simple.scm")
    
    // Experiment implementation
    
    def forFile(file: String): Unit = {
        val f = scala.io.Source.fromFile(file)
        val content: String    = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        f.close()
        val program: SchemeExp = AtomlangParser.parse(content)
        val results = configurations.map(executeExperiment(program, _))
        val statistics = results.map{case (name, measurements) =>
            val times = measurements.map(_._1)
            val mean = times.sum / times.length
            val stdv = Math.sqrt(times.map(t => (t - mean) * (t - mean)).sum / times.length)
            (name, measurements, mean, stdv)
        }
        printStatistics(file, statistics)
    }
    
    def executeExperiment(program: SchemeExp, configuration: Configuration): (String, List[Measurement]) = {
        val machine = configuration._2
        val graph   = DotGraph[machine.State, machine.Transition]()
        
        @scala.annotation.tailrec
        def iterate(n: Int, measurements: List[Measurement]): (String, List[Measurement]) = {
            if (n == 0) return (configuration._1, measurements.reverse)
            val t0 = System.nanoTime()
            val rs = machine.run[graph.G](program, timeout)
            val t1 = System.nanoTime()
            val ms = ((t1 - t0) / 1000000).toDouble
            val st = rs.nodes
            rs.toFile(n.toString + ".dot")
            Dot.toImage(n.toString + ".dot")
            iterate(n - 1, (ms, st) +: measurements)
        }
        
        iterate(iterations, List())
    }
    
    def printStatistics(file: String, statistics: List[(String, List[(Double, Int)], Double, Double)]): Unit = {
        println("\n***** " + file + " *****")
        println(" name  | mean | stdev | raw")
        statistics.foreach(s => println(s._1 + " " +  s._3) + " " + s._4 + " " + " " + s._2)
    }
    
    benchmarks.foreach(forFile)
}
