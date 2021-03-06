package scalaam.bench

import java.io.{BufferedWriter, FileWriter}

import au.com.bytecode.opencsv.CSVWriter
import scalaam.bench.BenchConfig._
import scalaam.core._
import scalaam.graph.DotGraph
import scalaam.language.LanguagePrelude
import scalaam.language.LanguagePrelude.Prelude.Prelude
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp}
import scalaam.lattice.Type

import scalaam.machine.IncAtomAnalysis

object BenchReuse {
  val address   = NameAddress
  val tid       = ExpTimeTID
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
  val incAtom = new IncAtomAnalysis[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.DeltaStore, sem, tid.Alloc())
  val graph = DotGraph[incAtom.State, incAtom.Transition]()

  var writer: CSVWriter = _

  def forFile(file: String, atPrelude: Prelude): Unit = {
    try {
      val f = scala.io.Source.fromFile(file)
      // Add the necessary preludes to the file contents.
      val content: String = LanguagePrelude.atomlangPrelude ++ LanguagePrelude.selectPrelude(atPrelude) ++ f.getLines.mkString("\n")
      f.close()
      val name               = file.split("/").last.dropRight(4) // DropRight removes ".scm".
      val program: SchemeExp = AtomlangParser.parse(content)
      val to                 = Timeout.seconds(timeout) // Start timer.
      incAtom.runWithLabels[graph.G](program, to) // Run benchmark.
      val sc = to.time      // Seconds passed.
      val re = sc > timeout // Check whether timeout has occurred.
      if (re) {
        val line: List[Any] = List(name, "timed out.")
        display("timeout")
        writer.writeNext(line.mkString(","))
        writer.flush()
      } else {
        val labels = incAtom.theLabels
        labels.foreach {
          case (int, mp) =>
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

  def main(args: Array[String]): Unit = {
    val output: String           = ts("Results_ReuseInc", ".csv")
    val fields: String           = "Benchmark,tid,(iteration,number)*" // Field names for the csv file.
    val out = new BufferedWriter(new FileWriter(output))
    writer = new CSVWriter(out)

    writer.writeNext(fields)
    writer.flush()
    display("Benchmark,tid,(iteration,number)*")
    benchmarks.foreach(Function.tupled(forFile))
    writer.close()
    display("\n\n***** Finished *****\n")
  }
}
