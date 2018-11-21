package scalaam

import scala.machine.ConcreteMachine

object Main {
    def main(args: Array[String]) = {
        ()
    }
    
    def lambda() = {
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
        val graph = DotGraph[machine.State, machine.Transition]
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
        val img = dot.substring(0, dot.length - 3) + "png"
        val base = "dot -Tpng " + dot + " -o " + img
        val cmd = if (System.getProperty("os.name").toLowerCase.contains("windows")) "bash -c \"" + base + "\"" else base
        cmd.!
    }
}

/* To be used with the console: `sbt console`, then scalaam.SchemeRun.run(file) */
object SchemeRun {
    
    import scalaam.core._
    import scalaam.graph._
    import scalaam.language.scheme._
    import scalaam.lattice._
    import scalaam.machine._
    
    val address = NameAddress
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp,
        address.A,
        Type.S,
        Type.B,
        Type.I,
        Type.R,
        Type.C,
        Type.Sym]
    val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
        address.Alloc[timestamp.T, SchemeExp])
    val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](StoreType.BasicStore, sem)
    val graph = DotGraph[machine.State, machine.Transition]
    
    def run(file: String, timeout: Timeout.T = Timeout.seconds(10)) = {
        val f = scala.io.Source.fromFile(file)
        val content = f.getLines.mkString("\n")
        val t0 = System.nanoTime
        val result = machine.run[graph.G](
            SchemeParser.parse(content),
            timeout)
        val t1 = System.nanoTime
        if (timeout.reached) {
            println("Time out!")
        } else {
            println(s"Time: ${(t1 - t0) / 1000000}ms")
        }
        f.close()
        result.toFile("foo.dot")
        import Graph.GraphOps
        println(s"States: ${result.nodes}")
        result
    }
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunAAM.run(file) */
object AtomlangRunAAM {
    
    import scalaam.core._
    import scalaam.graph._
    import scalaam.language.atomlang._
    import scalaam.language.scheme._
    import scalaam.lattice._
    import scalaam.machine._
    
    val address = NameAddress
    val timestamp = ZeroCFA[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp,
        address.A,
        Type.S,
        Type.B,
        Type.I,
        Type.R,
        Type.C,
        Type.Sym]
    val sem = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
        address.Alloc[timestamp.T, SchemeExp])
    val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](StoreType.BasicStore, sem)
    val graph = DotGraph[machine.State, machine.Transition]
    
    def run(file: String, out: String = "foo.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRunAAM.graph.G = {
        val f = scala.io.Source.fromFile(file)
        val content = f.getLines.mkString("\n")
        val t0 = System.nanoTime
        val result = machine.run[graph.G](
            SchemeParser.parse(content),
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
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRunConcrete.run(file) */
object AtomlangRunConcrete {
    
    import scalaam.core._
    import scalaam.language.atomlang._
    import scalaam.language.scheme._
    import scalaam.lattice._
    
    val address = ConcreteAddress
    val timestamp = ConcreteTimestamp[SchemeExp]()
    val lattice = new MakeSchemeLattice[SchemeExp,
        address.A,
        Concrete.S,
        Concrete.B,
        Concrete.I,
        Concrete.R,
        Concrete.C,
        Concrete.Sym]
    val sem = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp](address.Alloc[timestamp.T, SchemeExp])
    val machine = new ConcreteMachine[SchemeExp, address.A, lattice.L, timestamp.T](StoreType.CountingStore, sem)
    
    /**
      * Evaluate an Atomlang expression. Prints the result to out.
      *
      * @param input   A path to a file or a string representing code.
      * @param timeout A timeout value. Evaluation will stop after the timeout has been reached.
      */
    def run(input: String, timeout: Timeout.T = Timeout.seconds(10)): Unit = input match {
        case "" => System.err.print("No input to be run.")
        case _ if input.startsWith("test/Atomlang/") => // Files.exists(Paths.get(input))
            val f = scala.io.Source.fromFile(input)
            val content = f.getLines.mkString("\n")
            f.close()
            machine.eval(SchemeParser.parse(content), timeout).print()
        case _ => machine.eval(SchemeParser.parse(input), timeout).print()
    }
}