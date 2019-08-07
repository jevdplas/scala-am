package scalaam.bench

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.control.Breaks._
import au.com.bytecode.opencsv.CSVWriter
import scalaam.{IncAtomRun, Sem, StandardPrelude}
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
       // writer.writeNext(text)
       // writer.flush()
    }
    
    def forFile(file: String): Unit = {
        displayf("\n" ++ file ++ " ")
        try {
            val f = scala.io.Source.fromFile(file)
            // Add the necessary preludes to the file contents.
            val content: String = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
            f.close()
            compare(content)
        } catch {
            case e: Throwable => e.printStackTrace()
        }
    }
    
    def compare(content: String): Unit = {
        display("Concrete")
        val (_, success) = loopConcrete(content)
        if (success < repetitions) { // This threshold can be modified.
            display(" -> timed out.")
            return
        }
        display("\nAbstract")
        val abs = logAbstract(content)
        if (abs.isEmpty) {
            display(" -> timed out.")
            return
        }
    }
    
    def loopConcrete(content: String): (Map[Identifier, lattice.L], Int) = {
        var acc: Map[Identifier, lattice.L] = Map.empty.withDefaultValue(concrete.lattice.bottom)
        var successes = 0
        // Run the concrete machine "repetitions" times.
        for (i <- 1 to repetitions) {
            display(s" $i")
            logConcrete(content) match {
                case None => break
                case Some(map) =>
                    map.foreach { case (id, vl) =>
                        acc = acc + (id -> concrete.lattice.join(vl, acc(id)))
                    }
                    successes += 1
            }
        }
        //println(s"Succeeded $successes times.")
        //acc.foreach(v => println(s"${v._1} => ${v._2}"))
        (acc, successes)
    }
    
    // Logging for the concrete machine.
    def logConcrete(content: String): Option[Map[Identifier, lattice.L]] = {
        val t = Timeout.seconds(timeout)
        val result = concrete.run[graph.G](AtomlangParser.parse(content), t, Strategy.RandomInterleaving) // Important: random interleavings!
        if (t.reached) return None
        val states = result._nodes
        val map = states.foldLeft(Map[Identifier, lattice.L]().withDefaultValue(concrete.lattice.bottom))((curr, state) => {
            val store = state.store
            val busy = state.threads.busy // Threads that do not yet have finished.
            val contexts = busy.values.flatten
            contexts.foldLeft(curr)((curr, context) => {
                val control = context.control
                control match {
                    // Only look at states that evaluate an identifier.
                    case ControlEval(SchemeVar(id), env) =>
                        env.lookup(id.name) match {
                            case None       => curr // Unbound variable. TODO can we ignore this? (We do this also in logvalues ~> ScalaAM.scala.)
                            case Some(addr) =>
                                val value = store.lookup(addr).getOrElse(concrete.lattice.bottom)
                                val stored = curr(id)
                                curr + (id -> concrete.lattice.join(value, stored)) // Merge all values corresponding to the identifier.
                        }
                    case _ => curr
                }
            })
        })
        Some(map)
    }
    
    // Logging for the abstract machine.
    def logAbstract(content: String): Option[Map[Identifier, Sem.lattice.L]] = {
        val t = Timeout.seconds(timeout)
        val result = IncAtomRun.logValues(content, t)
        if (t.reached) return None
        result.foreach(v => println(s"${v._1} => ${v._2}"))
        Some(result)
    }
    
    // Convert the map outputted by the concrete machine so it uses the same lattice than the map outputted by the abstract machine.
    def convertMap(map: Map[Identifier, lattice.L]): Map[Identifier, Sem.lattice.L] = {
        map.mapValues(convertLattice)
    }
    
    def convertLattice(value: lattice.L): Sem.lattice.L = {
        ???
    }
    
    def main(args: Array[String]): Unit = {
        val    now: Date = Calendar.getInstance().getTime
        val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
        val output: String = "./Results_Soundness_Concrete" + format.format(now) + ".txt"
        
        val out = new BufferedWriter(new FileWriter(output))
        writer  = new CSVWriter(out)
    
        List("./test/Atomlang/Threads/abp.scm").foreach(forFile)
        writer.close()
    }
}