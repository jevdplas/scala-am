package scalaam.bench

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import au.com.bytecode.opencsv.CSVWriter
import scalaam.StandardPrelude
import scalaam.bench.BenchConfig.Prelude.Prelude
import scalaam.bench.BenchConfig._
import scalaam.lattice.Concrete
import scalaam.machine.{ConcurrentAAM, Strategy}
import scalaam.core._
import scalaam.language.atomlang._
import scalaam.language.scheme._
import scalaam.graph.DotGraph

object BenchSoundnessUsingConcrete {
    
    // Concrete setup.
    val address   = ConcreteAddress
    val tid       = ConcreteTID
    val timestamp = ConcreteTimestamp[SchemeExp]()
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    val concrete  = new ConcurrentAAM[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val graph     = DotGraph[concrete.State, concrete.Transition]()
    
    import concrete._
    
    // Experimental setup.
    val repetitions = 1//100 // Number of concrete experiments to run.
    var writer: CSVWriter = _
    
    def displayf(text: String): Unit = {
        display(text)
        writer.writeNext(text)
        writer.flush()
    }
    
    def forFile(file: String, atPrelude: Prelude): Unit = {
        display("\n" ++ file ++ " ")
        displayf("\n" ++ file ++ " ")
        try {
            val f = scala.io.Source.fromFile(file)
            // Add the necessary preludes to the file contents.
            val content: String = StandardPrelude.atomlangPrelude ++ (atPrelude match {
                case Prelude.lock => lockPrelude
                case Prelude.list => listPrelude
                case Prelude.none => ""
            }) ++ f.getLines.mkString("\n")
            f.close()
            compare(content)
        } catch {
            case e: Throwable => e.printStackTrace()
        }
    }
    
    def compare(content: String): Unit = {
        var acc: Map[Identifier, lattice.L] = Map.empty.withDefaultValue(concrete.lattice.bottom)
        // Run the concrete machine "repetitions" times.
        for (_ <- 1 to repetitions) {
            logValues(content, Timeout.seconds(timeout)).foreach { case (id, vl) =>
                acc = acc + (id -> concrete.lattice.join(vl, acc(id)))
            }
        }
        acc.foreach(v => println(s"${v._1} => ${v._2}"))
    }
    
    // Logging for the concrete machine.
    def logValues(content: String, timeout: Timeout.T = Timeout.seconds(10)): Map[Identifier, lattice.L] = {
        val result = concrete.run[graph.G](AtomlangParser.parse(content), timeout, Strategy.RandomInterleaving) // Important: random interleavings!
        val states = result._nodes
        states.foldLeft(Map[Identifier, lattice.L]().withDefaultValue(concrete.lattice.bottom))((curr, state) => {
            val store = state.store
            val busy = state.threads.busy // Threads that do not yet have finished.
            val contexts = busy.values.flatten
            contexts.foldLeft(curr)((curr, context) => {
                val control = context.control
                control match {
                    // Only look at states that evaluate an identifier.
                    case ControlEval(SchemeVar(id), env) =>
                        val addr = env.lookup(id.name).get
                        val value = store.lookup(addr).getOrElse(concrete.lattice.bottom)
                        val stored = curr(id)
                        curr + (id -> concrete.lattice.join(value, stored)) // Merge all values corresponding to the identifier.
                    case _ => curr
                }
            })
        })
    }
    
    def main(args: Array[String]): Unit = {
        val    now: Date = Calendar.getInstance().getTime
        val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
        val output: String = "./Results_Soundness_Concrete" + format.format(now) + ".txt"
        
        val out = new BufferedWriter(new FileWriter(output))
        writer  = new CSVWriter(out)
        
        benchmarks.foreach(Function.tupled(forFile))
        writer.close()
    }
}