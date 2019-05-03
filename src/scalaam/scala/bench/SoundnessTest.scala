package scala.bench

import scalaam.StandardPrelude
import scalaam.core._
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp}
import scalaam.lattice.Type
import sys.process._

import scala.machine.{ConcurrentModularRec, OptimisedIncConcModRec}

object SoundnessTest extends App {
    
    import scalaam.bench.BenchConfig.Prelude._
    import scalaam.bench.BenchConfig._
    
    val address   = NameAddress
    val tid       = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S,  Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    
    val cncMOD = new ConcurrentModularRec[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val incOPT = new OptimisedIncConcModRec[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    
    val timeout: Int = 10 * 60 // 10 minutes
    
    def forFile(file: String, atPrelude: Prelude): Int = {
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
            val name = file.split("/").last.dropRight(4) // DropRight removes ".scm".
            val program: SchemeExp = AtomlangParser.parse(content)
            display("cnc ")
            cncMOD.run(program, Timeout.seconds(timeout), name)
            display("inc ")
            incOPT.run(program, Timeout.seconds(timeout), name)
            display("\t-> comparing")
            val base: String = s"./recordings/run.sh ./recordings/$name"
            val command: String = if (System.getProperty("os.name").toLowerCase.contains("windows")) "bash -c \"" + base + "\"" else base
            command.!
        } catch {
            case e: Throwable => e.printStackTrace(); -1
        }
    }
    
    List(("./test/Atomlang/Threads/count.scm", lock)).foreach(Function.tupled(forFile))
}
