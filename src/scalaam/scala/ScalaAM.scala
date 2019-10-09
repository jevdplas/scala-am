package scala

import scalaam.bench.BenchSoundnessUsingConcrete.CSem
import scalaam.core._
import scalaam.graph.{DotGraph, Graph}
import scalaam.language.atomlang.{AtomlangParser, AtomlangSemantics}
import scalaam.language.scheme.{MakeSchemeLattice, SchemeExp}
import scala.lattice.{Concrete, Type}
import scala.machine.{ConcreteConcurrentAAM, ConcurrentAAM, Strategy}
import scala.machine.Strategy.Strategy
import Sem._
import scalaam.language.LanguagePrelude

import scala.machine._

object Dot {

  /**
    * Uses graphviz to create a .svg image of a given .dot file.<br>
    * Requires graphviz to be installed. On windows, requires WSL to be available.
    */
  def toImage(dot: String): Int = {
    if (!dot.endsWith(".dot")) {
      System.err.println("Cannot visualise: input was not a .dot file.")
      return -1
    }
    import sys.process._
    val img  = dot.substring(0, dot.length - 3) + "svg"
    val base = "dot -Tsvg " + dot + " -o " + img
    val cmd =
      if (System.getProperty("os.name").toLowerCase.contains("windows")) "bash -c \"" + base + "\""
      else base
    cmd.!
  }
}

object Sem {
  val address   = NameAddress
  val tid       = ExpTimeTID
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
}

object AAMRun {

  val machine = new ConcurrentAAM[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.DeltaStore, sem, tid.Alloc())
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "AAMRunResult.dot", timeout: Timeout.T = Timeout.seconds(10), strategy: Strategy = Strategy.AllInterleavings): AAMRun.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[AAMRun.graph.G]
}

/* To be used with the console: `sbt console`, then scalaam.ModAtomRun.run(file) */
object ModAtomRun {

  val machine = new ModAtomAnalysis[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.DeltaStore, sem, tid.Alloc())
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "ModAtomRunResult.dot", timeout: Timeout.T = Timeout.seconds(10)): ModAtomRun.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[ModAtomRun.graph.G]

  /*
  def logValues(
      content: String,
      timeout: Timeout.T = Timeout.seconds(10)
  ): Map[Identifier, Sem.lattice.L] = {
    val result = machine.run[graph.G](AtomlangParser.parse(content), timeout)
    def evalVar = new PartialFunction[machine.State, (Identifier, Sem.lattice.L)] {
      def apply(s: machine.State): (Identifier, Sem.lattice.L) = s.control match {
        case machine.ControlEval(SchemeVar(id), env) =>
          // println(s"env is $env")
          // println(s"store is ${machine.theStore}")
          val addr =
            env.lookup(id.name).getOrElse(throw new Exception(s"Unbound identifier: ${id.name}"))
          val v = machine.theStore
            .lookup(addr)
            .getOrElse(throw new Exception(s"Unbound address: $addr from ${id.pos}"))
          (id, v)
      }

      def isDefinedAt(s: machine.State): Boolean = s.control match {
        case machine.ControlEval(SchemeVar(_), _) => true
        case _                                    => false
      }
    }
    result
    /* Let's collect all nodes that evaluate a variable */
      .findNodes(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              env.lookup(id.name) match {
                case Some(_) => true
                case None    =>
                  // println(s"Identifier is unbound: $id")
                  false
              }
            case _ => false
          }
      )
      /* And evaluate the value of each variable */
      .collect(evalVar)
      /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
      .foldLeft(
        Map
          .empty[Identifier, Sem.lattice.L]
          .withDefaultValue(SchemeLattice[Sem.lattice.L, SchemeExp, address.A].bottom)
      )(
        (map, pair) =>
          pair match {
            case (id, value) =>
              map + (id -> SchemeLattice[Sem.lattice.L, SchemeExp, address.A].join(map(id), value))
          }
      )
  } */
}

/* To be used with the console: `sbt console`, then scalaam.IncAtomRun.run(file) */
object IncAtomRun {

  val machine = new IncAtomAnalysis[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](
    StoreType.DeltaStore,
    sem,
    tid.Alloc()
  )
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "IncAtomRunResult.dot", timeout: Timeout.T = Timeout.seconds(10)): IncAtomRun.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[IncAtomRun.graph.G]

  /*
  def logValues(
      content: String,
      timeout: Timeout.T = Timeout.seconds(10)
  ): Map[Identifier, Sem.lattice.L] = {
    val result = machine.run[graph.G](AtomlangParser.parse(content), timeout)
    def evalVar = new PartialFunction[machine.State, (Identifier, Sem.lattice.L)] {
      def apply(s: machine.State): (Identifier, Sem.lattice.L) = s.control match {
        case machine.ControlEval(SchemeVar(id), env) =>
          val addr =
            env.lookup(id.name).getOrElse(throw new Exception(s"Unbound identifier: ${id.name}"))
          val v =
            machine.theStore.lookup(addr).getOrElse(throw new Exception(s"Unbound address: $addr"))
          (id, v)
      }

      def isDefinedAt(s: machine.State): Boolean = s.control match {
        case machine.ControlEval(SchemeVar(_), _) => true
        case _                                    => false
      }
    }
    result
    /* Let's collect all nodes that evaluate a variable */
      .findNodes(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              env.lookup(id.name) match {
                case Some(_) => true
                case None    =>
                  // println(s"Identifier is unbound: $id")
                  false
              }
            case _ => false
          }
      )
      /* And evaluate the value of each variable */
      .collect(evalVar)
      /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
      .foldLeft(
        Map
          .empty[Identifier, Sem.lattice.L]
          .withDefaultValue(SchemeLattice[Sem.lattice.L, SchemeExp, address.A].bottom)
      )(
        (map, pair) =>
          pair match {
            case (id, value) =>
              map + (id -> SchemeLattice[Sem.lattice.L, SchemeExp, address.A].join(map(id), value))
          }
      )
  } */
}

object IncAtomThesis {

  val machine = new IncAtom[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.DeltaStore, sem, tid.Alloc())
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "IncAtomThesis.dot", timeout: Timeout.T = Timeout.seconds(10)): IncAtomThesis.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[IncAtomThesis.graph.G]
}

object ModAtomThesis {

  val machine = new ModAtom[SchemeExp, address.A, Sem.lattice.L, timestamp.T, tid.threadID](StoreType.DeltaStore, sem, tid.Alloc())
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "ModAtomThesis.dot", timeout: Timeout.T = Timeout.seconds(10)): ModAtomThesis.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[ModAtomThesis.graph.G]
}

object ConcreteRun {

  val address   = ConcreteAddress
  val tid       = ConcreteTID
  val timestamp = ConcreteTimestamp[SchemeExp]()
  val lattice   = new MakeSchemeLattice[SchemeExp, address.A,
    Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym](true) // Need concrete comparison!
  val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())

  val machine = new ConcreteConcurrentAAM[SchemeExp, CSem.address.A, CSem.lattice.L, CSem.timestamp.T, CSem.tid.threadID](StoreType.ConcreteStore, CSem.sem, CSem.tid.Alloc())
  val graph = DotGraph[machine.State, machine.Transition]()

  def run(file: String, out: String = "ConcreteRunResult.dot", timeout: Timeout.T = Timeout.seconds(10)): ConcreteRun.graph.G =
    RunUtil.run(file, machine, timeout, out).asInstanceOf[ConcreteRun.graph.G]

  def eval(expr: String, timeout: Timeout.T = Timeout.seconds(90)): ConcreteRun.graph.G =
    RunUtil.run(AtomlangParser.parse(LanguagePrelude.atomlangPrelude ++ expr), machine, timeout, "ConcreteEval.dot").asInstanceOf[ConcreteRun.graph.G]
}



object RunUtil {
  def readFile(file: String): SchemeExp = {
    val f   = scala.io.Source.fromFile(file)
    val exp = AtomlangParser.parse(LanguagePrelude.atomlangPrelude ++ f.getLines().mkString("\n"))
    f.close()
    exp
  }

  // TODO: Cleanup return type.
  def run[A <: Address, V, T, C](file: String, machine: MachineAbstraction[SchemeExp, A, V, T, C], timeout: Timeout.T, out: String): Any =
    run(readFile(file), machine, timeout, out)

  def run[A <: Address, V, T, C](exp: SchemeExp, machine: MachineAbstraction[SchemeExp, A, V, T, C], timeout: Timeout.T, out: String): Any = {
    val graph   = DotGraph[machine.State, machine.Transition]()
    val t0      = System.nanoTime
    val result  = machine.run[graph.G](exp, timeout)
    val t1      = System.nanoTime
    if (timeout.reached) {
      println("Time out!")
    } else {
      println(s"Time: ${(t1 - t0) / 1000000}ms")
    }
    result.toFile(out)
    import Graph.GraphOps
    println(s"States: ${result.nodes}")
    Dot.toImage(out)
    result
    /* Let's collect all nodes that evaluate a variable */
      .findNodes(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              env.lookup(id.name) match {
                case Some(_) => true
                case None    =>
                  // println(s"Identifier is unbound: $id")
                  false
              }
            case _ => false
          }
      )
      /* And evaluate the value of each variable */
      .collect(
        (s: machine.State) =>
          s.control match {
            case machine.ControlEval(SchemeVar(id), env) =>
              (id, s.store.lookup(env.lookup(id.name).get).get)
          }
      )
      /* We now have a list of pairs (variable, value).
         Let's join all of them by variable in a single map */
      .foldLeft(
        Map
          .empty[Identifier, lattice.L]
          .withDefaultValue(SchemeLattice[lattice.L, SchemeExp, address.A].bottom)
      )(
        (map, pair) =>
          pair match {
            case (id, value) =>
              map + (id -> SchemeLattice[lattice.L, SchemeExp, address.A].join(map(id), value))
          }
      )
  }
}

object SchemeRunAAMLKSS extends Interpreter {
  import scalaam.language.scheme._
  import scala.machine._
  import scalaam.core._
  import scalaam.graph._
  import scala.lattice._

  val address   = NameAddress
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice =
    new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
    address.Alloc[timestamp.T, SchemeExp]
  )
  val machine = new AAMLKSS[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(file: String, timeout: Timeout.T = Timeout.seconds(10), outputDot: Boolean = true) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val t0     = System.nanoTime
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }
}

object SchemeRunGAAM extends Interpreter {
  import scalaam.language.scheme._
  import scala.machine._
  import scalaam.core._
  import scalaam.graph._
  import scala.lattice._

  val address   = NameAddress
  val timestamp = ZeroCFA[SchemeExp]()
  val lattice =
    new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  val sem = new BaseSchemeSemantics[address.A, lattice.L, timestamp.T, SchemeExp](
    address.Alloc[timestamp.T, SchemeExp]
  )
  val machine = new GAAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = new DotGraph[machine.State, machine.Transition]

  def run(file: String, timeout: Timeout.T = Timeout.seconds(10), outputDot: Boolean = true) = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    val t0     = System.nanoTime
    val result = machine.run[graph.G](SchemeUndefiner.undefine(List(SchemeParser.parse(content))), timeout)
    val t1     = System.nanoTime
    val time   = (t1 - t0) / 1000000
    if (outputDot) result.toFile("foo.dot")
    import Graph.GraphOps
    val states = result.nodes
    (time, states)
  }
}

object CompareMachines {
  def compare(file: String): Unit = {
    println("Running concrete")
    val conc = SchemeRunConcrete.logValues(file)
    println("Running abstract")
    val abs = SchemeRunAAM.logValues(file)
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
    val conclat = SchemeLattice[SchemeRunConcrete.lattice.L, SchemeExp, SchemeRunConcrete.address.A]
    val abslat  = SchemeLattice[SchemeRunAAM.lattice.L, SchemeExp, SchemeRunAAM.address.A]
    conc.keySet.foreach(id => {
      val concval = conc(id)
      val abstractedconcval = conclat
        .concreteValues(concval)
        .foldLeft(abslat.bottom)(
          (lat, v) =>
            abslat.join(
              lat,
              v match {
                case ConcreteNumber(x) => abslat.number(x)
                case ConcreteReal(x)   => abslat.real(x)
                case ConcreteString(x) => abslat.string(x)
                case ConcreteBool(x)   => abslat.bool(x)
                case ConcreteChar(x)   => abslat.char(x)
                case ConcreteSymbol(x) => abslat.symbol(x)
                case ConcretePrim(prim: SchemeRunConcrete.sem.Primitive) =>
                  abslat
                    .primitive(SchemeRunAAM.sem.allPrimitives.find(p => p.name == prim.name).get)
                case ConcreteNil => abslat.nil
                case ConcreteClosure(exp, env, name) =>
                  val env2 = env.keys.foldLeft(Environment.empty[SchemeRunAAM.address.A])(
                    (env2, k) =>
                      env2.extend(k, env.lookup(k).get match {
                        case SchemeRunConcrete.address.A(nameaddr, _) => nameaddr
                      })
                  )
                  abslat.closure((exp.asInstanceOf[SchemeExp], env2), name)
                case ConcretePointer(SchemeRunConcrete.address.A(nameaddr, _)) =>
                  abslat.pointer(nameaddr)
              }
            )
        )
      val absval = abs(id)
      if (absval == abstractedconcval) {
        println(s"${id.fullString}: full precision! ($absval)")
      } else if (!abslat.subsumes(absval, abstractedconcval)) {
        println(
          s"${id.fullString}: SOUNDNESS PROBLEM, inferred $absval while concrete shows $abstractedconcval"
        )
      } else {
        println(
          s"${id.fullString}: overapproximative, inferred as $absval while best abstraction is $abstractedconcval"
        )
      }
    })
  }
}
