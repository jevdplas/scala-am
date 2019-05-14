package scala.bench

import scalaam.StandardPrelude
import scalaam.core._
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp}
import scalaam.lattice.Type

import sys.process._
import scala.machine.{ConcurrentModularRec, OptimisedIncConcModRec}

object BenchSoundness extends App {

    import scalaam.bench.BenchConfig.Prelude._
    import scalaam.bench.BenchConfig._
    
    val address   = NameAddress
    val tid       = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S,  Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    
    val cncMOD = new ConcurrentModularRec[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val incOPT = new OptimisedIncConcModRec[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    
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
            val t1 = Timeout.seconds(timeout)
            cncMOD.run(program, t1, name)
            val to1 = t1.reached
            if (to1) display(" timed out - ")
            display("inc ")
            val t2 = Timeout.seconds(timeout)
            incOPT.run(program, Timeout.seconds(timeout), name)
            val to2 = t2.reached
            if (to2) display("timed out - ")
            if (to1 && to2) return -3 // Both timed out, so no need to compare with sequential...
            display("\t-> comparing")
            val base: String = s"./recordings/run.sh ${file.drop(2)} $name"
            val command: String = if (System.getProperty("os.name").toLowerCase.contains("windows")) "bash -c \"" + base + "\"" else base
            command.!
        } catch {
            case e: Throwable => e.printStackTrace(); -1
        }
    }
    
    benchmarks.foreach(Function.tupled(forFile))
}
