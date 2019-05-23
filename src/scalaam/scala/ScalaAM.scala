package scalaam

import scalaam.core._
import scalaam.graph.{DotGraph, Graph}
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp, SchemeLattice, SchemeVar}
import scalaam.lattice.Type
import scalaam.machine.ConcreteMachine

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

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunModular.run(file) */
object AtomlangRunModular {
    
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
    val machine = new ConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
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
    
    def logValues(file: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, lattice.L] = {
        val f       = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        f.close()
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

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunModularIncremental.run(file) */
object AtomlangRunModularIncremental {
    
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
    val machine = new IncrementalConcurrentModular[SchemeExp, address.A, lattice.L, timestamp.T, tid.threadID](StoreType.BasicStore, sem, tid.Alloc())
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
    
    def logValues(file: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, lattice.L] = {
        val f       = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        f.close()
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
    
    def logValues(file: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, lattice.L] = {
        val f       = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        f.close()
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

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunConcrete.run(file) or scalaam.AtomlangRunConcrete.eval(input) */
object AtomlangRunConcrete {
    
    import scalaam.core._
    import scalaam.language.atomlang._
    import scalaam.language.scheme._
    import scalaam.lattice._
    
    val tid = ConcreteTID
    val timestamp = ConcreteTimestamp[SchemeExp]()
    val address = TimestampAddress[timestamp.T, SchemeExp]
    val lattice = new MakeSchemeLattice[SchemeExp,
        address.A,
        Concrete.S,
        Concrete.B,
        Concrete.I,
        Concrete.R,
        Concrete.C,
        Concrete.Sym]
    val sem = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc, tid.Alloc())
    val machine = new ConcreteMachine[SchemeExp, address.A, lattice.L, timestamp.T](StoreType.CountingStore, sem)
    val graph = DotGraph[machine.State, machine.Transition]()
    
    /**
      * Evaluate an Atomlang expression. Prints the result to out.
      *
      * @param input   A path to a file or a string representing code.
      * @param timeout A timeout value. Evaluation will stop after the timeout has been reached.
      */
    def eval(input: String, timeout: Timeout.T = Timeout.seconds(10)): Unit = {
        var t0 = 0L
        var t1 = 0L
        input match {
            case "" =>
                System.err.print("No input to be run.")
                return
            case _ if input.startsWith("test/Atomlang/") || input.startsWith("./test/Atomlang/") =>
                val f = scala.io.Source.fromFile(input)
                val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
                f.close()
                t0 = System.nanoTime
                machine.eval(AtomlangParser.parse(content), timeout).print()
                t1 = System.nanoTime
            case _ =>
                t0 = System.nanoTime
                machine.eval(AtomlangParser.parse(StandardPrelude.atomlangPrelude ++ input), timeout).print()
                t1 = System.nanoTime
        }
        if (timeout.reached) {
            println("\nTimed out!")
        } else {
            println(s"\nTime: ${(t1 - t0) / 1000000}ms")
        }
    }
    
    /**
      * Evaluate an Atomlang expression. Returns the resulting graph.
      *
      * @param file    A path to a file or a string representing code.
      * @param out     A path to which the dotgraph will be written.
      * @param timeout A timeout value. Evaluation will stop after the timeout has been reached.
      */
    def run(file: String, out: String = "AtomlangRunConcreteResult.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRunConcrete.graph.G = {
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
    
    def logValues(file: String, timeout: Timeout.T = Timeout.seconds(10)) : Map[Identifier, lattice.L] = {
        val f       = scala.io.Source.fromFile(file)
        val content = StandardPrelude.atomlangPrelude ++ f.getLines.mkString("\n")
        f.close()
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
                (id, s.store.lookup(env.lookup(id.name).get).get) // Modified since there is a global store.
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

object CompareMachines {
    def compare(file: String): Unit = {
        println("Running concrete")
        val conc = AtomlangRunConcrete.logValues(file)
        println("Running abstract")
        val abs = AtomlangRunModularIncremental.logValues(file)
        if (conc.keySet != abs.keySet) {
            if (conc.keySet.subsetOf(abs.keySet)) {
                println(s"Abstract has observed extra variables: ${abs.keySet.diff(conc.keySet)}")
            } else {
                println("!!!SOUNDNESS PROBLEM!!!")
                /* If the concrete execution observed variables not observed in the abstract, the abstract is not sound! */
                println(s"Concrete has observed extra variables: ${conc.keySet.diff(abs.keySet)}")
                return () /* And we can directly abort */
            }
        }
        import scalaam.core.ConcreteVal._
        import scalaam.language.scheme._
        val conclat = SchemeLattice[AtomlangRunConcrete.lattice.L, SchemeExp, AtomlangRunConcrete.address.A]
        val abslat = SchemeLattice[AtomlangRunModularIncremental.lattice.L, SchemeExp, AtomlangRunModularIncremental.address.A]
        conc.keySet.foreach(id => {
            val concval = conc(id)
            val abstractedconcval = conclat.concreteValues(concval).foldLeft(abslat.bottom)((lat, v) => abslat.join(lat, v match {
                case ConcreteNumber(x) => abslat.number(x)
                case ConcreteReal(x) => abslat.real(x)
                case ConcreteString(x) => abslat.string(x)
                case ConcreteBool(x) => abslat.bool(x)
                case ConcreteChar(x) => abslat.char(x)
                case ConcreteSymbol(x) => abslat.symbol(x)
                case ConcretePrim(p) => abslat.primitive(p)
                case ConcreteNil => abslat.nil
                case ConcreteFuture(t) => abslat.future(t)
              //  case ConcreteAtom(v) => abslat.atom(v)
                case ConcreteClosure(exp, env) =>
                    val env2 = env.keys.foldLeft(Environment.empty[AtomlangRunModularIncremental.address.A])((env2, k) =>
                        env2.extend(k, env.lookup(k).get match {
                            case AtomlangRunConcrete.address.A(nameaddr, _) => nameaddr
                        }))
                    abslat.closure((exp.asInstanceOf[SchemeExp], env2))
                case ConcretePointer(AtomlangRunConcrete.address.A(nameaddr, _)) =>
                    abslat.pointer(nameaddr)
            }))
            val absval = abs(id)
            if (absval == abstractedconcval) {
                println(s"$id: full precision! ($absval)")
            } else if (!abslat.subsumes(absval, abstractedconcval)) {
                println(s"$id: SOUNDNESS PROBLEM, inferred $absval while concrete shows $abstractedconcval")
            } else {
                println(s"$id: overapproximative, inferred as $absval while best abstraction is $abstractedconcval")
            }
        })
    }
}