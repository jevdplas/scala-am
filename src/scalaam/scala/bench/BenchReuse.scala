package scala.bench

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import au.com.bytecode.opencsv.CSVWriter
import scalaam.StandardPrelude
import scalaam.bench.BenchConfig.Prelude._
import scalaam.bench.BenchConfig._
import scalaam.core._
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp}
import scalaam.lattice.Type
import scalaam.graph.DotGraph

import scala.machine.IncrementalConcurrentModular

object BenchReuse extends App {
    
    val address   = NameAddress
    val tid       = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S,  Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    val modInc    = new IncrementalConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val graph     = DotGraph[modInc.State, modInc.Transition]()
    
    val now:                Date =  Calendar.getInstance().getTime
    val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
    val output:           String = "./Results_ReuseInc" + format.format(now) + ".csv"
    val fields:           String = "Benchmark,tid,(iteration,number)*" // Field names for the csv file.
    
    val    out = new BufferedWriter(new FileWriter(output))
    val writer = new CSVWriter(out)
    
    def forFile(file: String, atPrelude: Prelude): Unit = {
        try {
            val f = scala.io.Source.fromFile(file)
            // Add the necessary preludes to the file contents.
            val content: String = StandardPrelude.atomlangPrelude ++ (atPrelude match {
                case Prelude.lock => lockPrelude
                case Prelude.list => listPrelude
                case Prelude.none => ""
            }) ++ f.getLines.mkString("\n")
            f.close()
            val name = file.split("/").last.dropRight(4) // DropRight removes ".scm".
            val program: SchemeExp = AtomlangParser.parse(content)
            val to = Timeout.seconds(timeout) // Start timer.
            modInc.run[graph.G](program, to) // Run benchmark.
            val sc = to.time // Seconds passed.
            val re = sc > timeout // Check whether timeout has occurred.
            if (re) {
                val line: List[Any] = List(name, "timed out.")
                display("timeout")
                writer.writeNext(line.mkString(","))
                writer.flush()
            } else {
                val labels = modInc.theLabels
                labels.foreach{case (int, mp) =>
                    val line: List[Any] = name :: int :: mp.toList
                    display("\n" ++ name ++ "\t" ++ line.tail.mkString(","))
                    writer.writeNext(line.mkString(","))
                    writer.flush()
                }
            }
        } catch {
            case e: Throwable => e.printStackTrace()
        }
    }
    
    writer.writeNext(fields)
    writer.flush()
    display("Benchmark,tid,(iteration,number)*")
    benchmarks.foreach(Function.tupled(forFile))
    writer.close()
    display("\n\n***** Finished *****\n")
}
