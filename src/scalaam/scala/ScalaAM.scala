package scalaam

import scalaam.core._
import scalaam.graph.{DotGraph, Graph}
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp, SchemeLattice, SchemeVar}
import scalaam.lattice.Type

import scala.machine._

object Main {
    def main(args: Array[String]): Unit = {
        ()
    }
    
    def lambda(): Unit = {
        import scalaam.core._
        import scalaam.graph._
        import scalaam.language.lambda._
        import scalaam.machine._
        
        val address = NameAddress
        val timestamp = ZeroCFA[LambdaExp]()
        val lattice = LambdaSetLattice[address.A]()
        val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](
            address.Alloc[timestamp.T, LambdaExp])
        val machine = new AAM[LambdaExp, address.A, lattice.L, timestamp.T](StoreType.BasicStore, sem)
        val graph = DotGraph[machine.State, machine.Transition]()
        val result = machine.run[graph.G](
            LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"),
            Timeout.Infinity)
        result.toFile("foo.dot")
    }
}

object Dot {
    
    /**
      * Uses graphviz to create a .png image of a given .dot file.<br>
      * Requires graphviz to be installed. On windows, requires WSL to be available.
      */
    def toImage(dot: String): Int = {
        if (!dot.endsWith(".dot")) {
            System.err.println("Cannot visualise: input was not a .dot file.")
            return -1
        }
        import sys.process._
        val img = dot.substring(0, dot.length - 3) + "svg"
        val base = "dot -Tsvg " + dot + " -o " + img
        val cmd = if (System.getProperty("os.name").toLowerCase.contains("windows")) "bash -c \"" + base + "\"" else base
        cmd.!
    }
}

object Sem {
    
    import scalaam.core._
    import scalaam.language.atomlang._
    import scalaam.language.scheme._
    
    val address = NameAddress
    val tid = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
    val sem = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunModular.run(file) */
object AtomlangRunModular {
    
    import scalaam.graph._
    import Sem._
    
    val machine = new ConcurrentModular[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val graph = DotGraph[machine.State, machine.Transition]()
    
    def run(file: String, out: String = "AtomlangRunModularResult.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRunModular.graph.G = {
        val f = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        val t0 = System.nanoTime
        val result = machine.run[graph.G](
            AtomlangParser.parse(content),
            timeout)
        val t1 = System.nanoTime
        if (timeout.reached) {
            println("Time out!")
        } else {
            println(s"Time: ${(t1 - t0) / 1000000}ms")
        }
        f.close()
        result.toFile(out)
        import Graph.GraphOps
        println(s"States: ${result.nodes}")
        Dot.toImage(out)
        result
    }
    
    def logValues(content: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, Sem.lattice.L] = {
        val result = machine.run[graph.G](AtomlangParser.parse(content), timeout)
        result
            /* Let's collect all nodes that evaluate a variable */
            .findNodes((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                env.lookup(id.name) match {
                    case Some(_) => true
                    case None =>
                        // println(s"Identifier is unbound: $id")
                        false
                }
            case _ => false
        })
            /* And evaluate the value of each variable */
            .collect((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                (id, machine.theStore.lookup(env.lookup(id.name).get).get) // Modified since there is a global store.
        })
            /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
            .foldLeft(Map.empty[Identifier, Sem.lattice.L].withDefaultValue(SchemeLattice[Sem.lattice.L, SchemeExp, address.A].bottom))((map, pair) => pair match {
            case (id, value) => map + (id -> SchemeLattice[Sem.lattice.L, SchemeExp, address.A].join(map(id), value))
        })
    }
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunModularIncremental.run(file) */
object AtomlangRunModularIncremental {
    
    import Sem._
    
    val machine = new IncrementalConcurrentModular[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val graph = DotGraph[machine.State, machine.Transition]()
    
    def run(file: String, out: String = "AtomlangRunModularIncrementalResult.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRunModularIncremental.graph.G = {
        val f = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        val t0 = System.nanoTime
        val result = machine.run[graph.G](
            AtomlangParser.parse(content),
            timeout)
        val t1 = System.nanoTime
        if (timeout.reached) {
            println("Time out!")
        } else {
            println(s"Time: ${(t1 - t0) / 1000000}ms")
        }
        f.close()
        result.toFile(out)
        import Graph.GraphOps
        println(s"States: ${result.nodes}")
        Dot.toImage(out)
        result
    }
    
    def logValues(content: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, Sem.lattice.L] = {
        val result = machine.run[graph.G](AtomlangParser.parse(content), timeout)
        result
            /* Let's collect all nodes that evaluate a variable */
            .findNodes((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                env.lookup(id.name) match {
                    case Some(_) => true
                    case None =>
                        // println(s"Identifier is unbound: $id")
                        false
                }
            case _ => false
        })
            /* And evaluate the value of each variable */
            .collect((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                (id, machine.theStore.lookup(env.lookup(id.name).get).get) // Modified since there is a global store.
        })
            /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
            .foldLeft(Map.empty[Identifier, Sem.lattice.L].withDefaultValue(SchemeLattice[Sem.lattice.L, SchemeExp, address.A].bottom))((map, pair) => pair match {
            case (id, value) => map + (id -> SchemeLattice[Sem.lattice.L, SchemeExp, address.A].join(map(id), value))
        })
    }
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunModularIncrementalOptimised.run(file) */
object AtomlangRunModularIncrementalOptimised {
    
    import scalaam.core._
    import scalaam.graph._
    import scalaam.language.atomlang._
    import scalaam.language.scheme._
    import scalaam.lattice._
    
    val address = NameAddress
    val tid = ConcreteTID
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp,
        address.A,
        Type.S,
        Type.B,
        Type.I,
        Type.R,
        Type.C,
        Type.Sym]
    val sem = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    val machine = new OptimisedIncConcMod[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
    val graph = DotGraph[machine.State, machine.Transition]()
    
    def run(file: String, out: String = "AtomlangRunModularIncrementalResultOptimised.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRunModularIncrementalOptimised.graph.G = {
        val f = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        val t0 = System.nanoTime
        val result = machine.run[graph.G](
            AtomlangParser.parse(content),
            timeout)
        val t1 = System.nanoTime
        if (timeout.reached) {
            println("Time out!")
        } else {
            println(s"Time: ${(t1 - t0) / 1000000}ms")
        }
        f.close()
        result.toFile(out)
        import Graph.GraphOps
        println(s"States: ${result.nodes}")
        Dot.toImage(out)
        result
    }
    
    def logValues(content: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, lattice.L] = {
        val result = machine.run[graph.G](AtomlangParser.parse(content), timeout)
        result
            /* Let's collect all nodes that evaluate a variable */
            .findNodes((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                env.lookup(id.name) match {
                    case Some(_) => true
                    case None =>
                        // println(s"Identifier is unbound: $id")
                        false
                }
            case _ => false
        })
            /* And evaluate the value of each variable */
            .collect((s: machine.State) => s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
                (id, machine.theStore.lookup(env.lookup(id.name).get).get) // Modified since there is a global store.
        })
            /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
            .foldLeft(Map.empty[Identifier, lattice.L].withDefaultValue(SchemeLattice[lattice.L, SchemeExp, address.A].bottom))((map, pair) => pair match {
            case (id, value) => map + (id -> SchemeLattice[lattice.L, SchemeExp, address.A].join(map(id), value))
        })
    }
}

object StandardPrelude {
    val atomlangPrelude: String =
        """(define (swap! at fn)
          |  (let ((vl (read at)))
          |    (if (not (compare-and-set! at vl (fn vl)))
          |      (swap! at fn))))""".stripMargin
}