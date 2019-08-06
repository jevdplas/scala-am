package scala.bench

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import au.com.bytecode.opencsv.CSVWriter
import scalaam._
import scalaam.core._

object BenchSoundness {
    
    import scalaam.bench.BenchConfig.Prelude._
    import scalaam.bench.BenchConfig._
    import scalaam.language.scheme._
    
    val latt = SchemeLattice[Sem.lattice.L, SchemeExp, Sem.address.A]
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
        display("mod ")
        val t1 = Timeout.seconds(timeout)
        val mod = ModAtomRun.logValues(content, t1)
        if (t1.reached) {
            display("timed out\n")
            return
        }
        display(" - inc\n")
        val t2 = Timeout.seconds(timeout)
        val inc = IncAtomRun.logValues(content, t2)
        if (t2.reached) {
            display("timed out\n")
            return
        }
        if (mod.keySet != inc.keySet) {
            if (mod.keySet.subsetOf(inc.keySet)) {
                displayf(s"Abstract has observed extra variables: ${inc.keySet.diff(mod.keySet)}\n")
            } else {
                displayf("!!!SOUNDNESS PROBLEM!!!\n")
                /* If the concrete execution observed variables not observed in the abstract, the abstract is not sound! */
                displayf(s"Concrete has observed extra variables: ${mod.keySet.diff(inc.keySet)}\n")
                return /* And we can directly abort */
            }
        }
        
        mod.keySet.foreach(id => {
            val modval = mod(id)
            val incval = inc(id)
            if (incval == modval) {
                //                displayf(s"$id: full precision! ($incval)\n")
            } else if (!latt.subsumes(incval, modval)) {
                displayf(s"$id: SOUNDNESS PROBLEM, inferred $incval while concrete shows $modval\n")
            } else {
                displayf(s"$id: overapproximative, inferred as $incval while best abstraction is $modval\n")
            }
        })
    }
    
    def main(args: Array[String]): Unit = {
        val    now: Date = Calendar.getInstance().getTime
        val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
        val output: String = "./Results_Soundness" + format.format(now) + ".txt"
        
        val out = new BufferedWriter(new FileWriter(output))
        writer  = new CSVWriter(out)
        
        benchmarks.foreach(Function.tupled(forFile))
        writer.close()
    }
}