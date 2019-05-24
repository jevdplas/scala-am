package scala.bench

import scalaam._
import scalaam.core._

object BenchSoundness extends App {

    import scalaam.bench.BenchConfig.Prelude._
    import scalaam.bench.BenchConfig._
    import scalaam.language.scheme._
    
    val latt = SchemeLattice[Sem.lattice.L, SchemeExp, Sem.address.A]
    
    def forFile(file: String, atPrelude: Prelude): Unit = {
        display("\n" ++ file ++ " ")
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
        val mod = AtomlangRunModular.logValues(content, t1)
        if (t1.reached) {
            display("timed out\n")
            return
        }
        display(" - inc\n")
        val t2 = Timeout.seconds(timeout)
        val inc = AtomlangRunModularIncremental.logValues(content, t2)
        if (t2.reached) {
            display("timed out\n")
            return
        }
        if (mod.keySet != inc.keySet) {
            if (mod.keySet.subsetOf(inc.keySet)) {
                display(s"Abstract has observed extra variables: ${inc.keySet.diff(mod.keySet)}\n")
            } else {
                display("!!!SOUNDNESS PROBLEM!!!\n")
                /* If the concrete execution observed variables not observed in the abstract, the abstract is not sound! */
                display(s"Concrete has observed extra variables: ${mod.keySet.diff(inc.keySet)}\n")
                return /* And we can directly abort */
            }
        }
        
        mod.keySet.foreach(id => {
            val modval = mod(id)
            val incval = inc(id)
            if (incval == modval) {
                display(s"$id: full precision! ($incval)\n")
            } else if (!latt.subsumes(incval, modval)) {
                display(s"$id: SOUNDNESS PROBLEM, inferred $incval while concrete shows $modval\n")
            } else {
                display(s"$id: overapproximative, inferred as $incval while best abstraction is $modval\n")
            }
        })
    }
    
    benchmarks.foreach(Function.tupled(forFile))
}
