package scalaam.bench

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.control.Breaks._
import au.com.bytecode.opencsv.CSVWriter
import scalaam.StandardPrelude
import scalaam.bench.BenchConfig._
import scalaam.core.ConcreteAddress.Pointer
import scalaam.core.ConcreteVal._
import scalaam.lattice.{Concrete, Type}
import scalaam.machine.{ConcreteConcurrentAAM, Strategy}
import scalaam.core._
import scalaam.language.atomlang._
import scalaam.language.scheme._
import scalaam.graph.DotGraph

import scala.machine.IncAtom

object BenchSoundnessUsingConcrete {
    
    // Setup.
    
    object ASem {
        val address   = NameAddress
        val tid       = ExpTimeTID
        val timestamp = ZeroCFA[SchemeExp]()
        val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
        val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    }

    object CSem {
        val address   = ConcreteAddress
        val tid       = ConcreteTID
        val timestamp = ConcreteTimestamp[SchemeExp]()
        val lattice   = new MakeSchemeLattice[SchemeExp, address.A, Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym](true) // Need concrete comparison!
        val sem       = new AtomlangSemantics[address.A, lattice.L, timestamp.T, SchemeExp, tid.threadID](address.Alloc[timestamp.T, SchemeExp], tid.Alloc())
    }
    
    val concrete  = new ConcreteConcurrentAAM[SchemeExp, CSem.address.A, CSem.lattice.L, CSem.timestamp.T, CSem.tid.threadID](StoreType.ConcreteStore, CSem.sem, CSem.tid.Alloc())
    val cGraph    = DotGraph[concrete.State, concrete.Transition]()
    val clat      = SchemeLattice[CSem.lattice.L, SchemeExp, CSem.address.A]
    
    val incAtom   = new IncAtom[SchemeExp, ASem.address.A, ASem.lattice.L, ASem.timestamp.T, ASem.tid.threadID](StoreType.DeltaStore, ASem.sem, ASem.tid.Alloc())
    val aGraph    = DotGraph[incAtom.State, incAtom.Transition]()
    val alat      = SchemeLattice[ASem.lattice.L, SchemeExp, ASem.address.A]
    
    // Configuration.
    val repetitions  = 25//500 // Number of concrete experiments to run.
    val timeout: Int = 30 * 60 // 10 minutes
    val benchmarks: List[String] = List(
        "./test/Atomlang/VMCAI2020/actors.scm",
        "./test/Atomlang/VMCAI2020/atomicInt.scm",
        "./test/Atomlang/VMCAI2020/atoms.scm",
        "./test/Atomlang/VMCAI2020/bchain.scm",
        "./test/Atomlang/VMCAI2020/count.scm",
        "./test/Atomlang/VMCAI2020/dekker.scm",
        "./test/Atomlang/VMCAI2020/fact.scm",
        "./test/Atomlang/VMCAI2020/life.scm",
        "./test/Atomlang/VMCAI2020/matmul.scm",
        "./test/Atomlang/VMCAI2020/mcarlo.scm",
        "./test/Atomlang/VMCAI2020/mceval.scm",
        "./test/Atomlang/VMCAI2020/minimax.scm",
        "./test/Atomlang/VMCAI2020/msort.scm",
        "./test/Atomlang/VMCAI2020/pc.scm",
        "./test/Atomlang/VMCAI2020/treiber-stack.scm",
    )
    
    // Setup.
    var writer: CSVWriter = _
    
    def displayf(text: String): Unit = {
        writer.writeNext(text)
        writer.flush()
    }
    
    def forFile(file: String): Unit = {
        display("\n" ++ file)
        displayf("\n" ++ file)
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
    
    def compare(content: String): Unit =
    try {
        display("\nConcrete")
        val (concrete, success) = loopConcrete(content)
        if (success < repetitions) { // This threshold can be modified.
            display(" -> timed out.\n")
            return
        }
        display("\nAbstract")
        val abs = logAbstract(content) match {
            case None =>
                display(" -> timed out.\n")
                return
            case Some(map) => map
        }

        display("\nStarting comparison.")
        
        if (concrete.keySet != abs.keySet) {
            if (concrete.keySet.subsetOf(abs.keySet)) {
                display (s"\nAbstract has observed extra variables: ${abs.keySet.diff(concrete.keySet)}")
                displayf(s"Abstract has observed extra variables: ${abs.keySet.diff(concrete.keySet)}")
            }
            else {
                display (s"\nUNSOUND: concrete has observed extra variables: ${concrete.keySet.diff(abs.keySet)}")
                displayf(s"UNSOUND: concrete has observed extra variables: ${concrete.keySet.diff(abs.keySet)}")
                return
            }
        }
        
        val abstracted = convertMap(concrete)
        
        concrete.keySet.foreach(id => {
            val concr = abstracted(id)
            val absv = abs(id)
            if (concr == absv)
                displayf(s"$id - OK: $concr")
            else if (!alat.subsumes(absv, concr)) {
                display(s"\n$id - UNSOUND: inferred $absv where required $concr.")
                displayf(s"$id - UNSOUND: inferred $absv where required $concr.")
            }
            else {
                display (s"\n$id - Precision loss: inferred $absv where $concr suffices.")
                displayf(s"$id - Precision loss: inferred $absv where $concr suffices.")
            }
        })

        display("\nComparison finished.")
        
    } catch {
        case e: Throwable =>
            e.printStackTrace()
            displayf(e.getStackTrace.toString)
    }
    
    def loopConcrete(content: String): (Map[Identifier, CSem.lattice.L], Int) = {
        var acc: Map[Identifier, CSem.lattice.L] = Map.empty.withDefaultValue(concrete.lattice.bottom)
        var successes = 0
        // Run the concrete machine "repetitions" times.
        breakable {
            for (_ <- 1 to repetitions) {
                display(" *")
                logConcrete(content) match {
                    case None => break
                    case Some(map) =>
                        map.foreach { case (id, vl) =>
                            acc = acc + (id -> concrete.lattice.join(vl, acc(id)))
                        }
                        successes += 1
                }
            }
        }
        display(s" ~> Succeeded $successes/$repetitions times.\n")
        //acc.foreach(v => println(s"${v._1} => ${v._2}"))
        (acc, successes)
    }
    
    // Logging for the concrete machine.
    def logConcrete(content: String): Option[Map[Identifier, CSem.lattice.L]] = {
        val t = Timeout.seconds(timeout)
        val result = concrete.run[cGraph.G](AtomlangParser.parse(content), t, Strategy.RandomInterleaving) // Important: random interleavings!
        if (t.reached) return None
        val states = result._nodes
        val map = states.foldLeft(Map[Identifier, CSem.lattice.L]().withDefaultValue(concrete.lattice.bottom))((curr, state) => {
            val store = state.store
            val busy = state.threads.runnable ++ state.threads.blocked.mapValues(_.map(_._2)) // Threads that do not yet have finished. // TODO: should this really include the threads that are blocked?
            val contexts = busy.values.flatten
            contexts.foldLeft(curr)((curr, context) => {
                val control = context.control
                control match {
                    // Only look at states that evaluate an identifier.
                    case concrete.ControlEval(SchemeVar(id), env) =>
                        env.lookup(id.name) match {
                            case None       => curr // Unbound variable. TODO: can we ignore this? (We do this also in logValues ~> ScalaAM.scala.)
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
    def logAbstract(content: String): Option[Map[Identifier, ASem.lattice.L]] = {
        val t = Timeout.seconds(timeout)
        val result = incAtom.run[aGraph.G](AtomlangParser.parse(content), t)
        if (t.reached) return None
        def evalVar = new PartialFunction[incAtom.State, (Identifier, ASem.lattice.L)] {
            def apply(s: incAtom.State): (Identifier, ASem.lattice.L)  = s.control match {
                case incAtom.ControlEval(SchemeVar(id), env) =>
                    val addr = env.lookup(id.name).getOrElse(throw new Exception(s"Unbound identifier: ${id.name}"))
                    val v = incAtom.theStore.lookup(addr).getOrElse(throw new Exception(s"Unbound address: $addr"))
                    (id, v)
            }
        
            def isDefinedAt(s: incAtom.State): Boolean = s.control match {
                case incAtom.ControlEval(SchemeVar(_), _) => true
                case _ => false
            }
        }
        val res = result
            /* Let's collect all nodes that evaluate a variable */
            .findNodes((s: incAtom.State) => s.control match {
            case incAtom.ControlEval(SchemeVar(id), env) =>
                env.lookup(id.name) match {
                    case Some(_) => true
                    case None =>
                        // println(s"Identifier is unbound: $id")
                        false
                }
            case _ => false
        })
            /* And evaluate the value of each variable */
            .collect(evalVar)
            /* We now have a list of pairs (variable, value).
               Let's join all of them by variable in a single map */
            .foldLeft(Map.empty[Identifier, ASem.lattice.L].withDefaultValue(SchemeLattice[ASem.lattice.L, SchemeExp, ASem.address.A].bottom))((map, pair) => pair match {
            case (id, value) => map + (id -> SchemeLattice[ASem.lattice.L, SchemeExp, ASem.address.A].join(map(id), value))
        })
        Some(res)
    }
    
    // Convert the map outputted by the concrete machine so it uses the same lattice than the map outputted by the abstract machine.
    def convertMap(map: Map[Identifier, CSem.lattice.L]): Map[Identifier, ASem.lattice.L] = {
        map.mapValues(convertLattice)
    }
    
    def convertLattice(lat: CSem.lattice.L): ASem.lattice.L = {
        clat.concreteValues(lat).map(convertValue).fold(alat.bottom)(alat.join(_,_))
    }
    
    def convertValue(value: ConcreteVal): ASem.lattice.L = value match {
        case ConcreteNumber(x) => alat.number(x)
        case ConcreteReal(x) => alat.real(x)
        case ConcreteString(x) => alat.string(x)
        case ConcreteBool(x) => alat.bool(x)
        case ConcreteChar(x) => alat.char(x)
        case ConcreteSymbol(x) => alat.symbol(x)
        case ConcretePrim(p: CSem.sem.Primitive) => alat.primitive(ASem.sem.allPrimitives.find(_.name == p.name).get)
        case ConcreteNil => alat.nil
        case ConcreteClosure(exp, env) =>
            val env2 = env.keys.foldLeft(Environment.empty[ASem.address.A])((env2, k) =>
                env2.extend(k, env.lookup(k).get match {
                    case CSem.address.Variable(nameAddr, _) => ASem.address.Variable(nameAddr)
                    case CSem.address.Pointer(e, _) => ASem.address.Pointer(e)
                    case CSem.address.Primitive(n) => ASem.address.Primitive(n)
                }))
            alat.closure((exp.asInstanceOf[SchemeExp], env2))
        case ConcretePointer(CSem.address.Variable(nameAddr, _)) =>
            alat.pointer(ASem.address.Variable(nameAddr))
        case ConcretePointer(Pointer(name, _)) =>
            alat.pointer(ASem.address.Pointer(name))
        case ConcreteFuture(CSem.tid.TID(exp, _, _)) => alat.future(ASem.tid.TID(exp, ASem.timestamp.T.typeclass.initial(""))) // Mimick ZeroCFA timestamp.
        case ConcreteCons(car, cdr) =>
            val ccar = car.map(convertValue).fold(alat.bottom)((x, y) => alat.join(x, y))
            val ccdr = cdr.map(convertValue).fold(alat.bottom)((x, y) => alat.join(x, y))
            alat.cons(ccar, ccdr)
        case ConcreteAtom(data) =>
            val cdata = data.map(convertValue).fold(alat.bottom)((x, y) => alat.join(x, y))
            alat.atom(cdata)
    }
    
    def main(args: Array[String]): Unit = {
        val    now: Date = Calendar.getInstance().getTime
        val format: SimpleDateFormat = new SimpleDateFormat("_yyyy-MM-dd-HH'h'mm")
        val output: String = "./Results_Soundness_Concrete" + format.format(now) + ".txt"
        
        val out = new BufferedWriter(new FileWriter(output))
        writer  = new CSVWriter(out, ',', CSVWriter.NO_QUOTE_CHARACTER)
    
        benchmarks.foreach(forFile)
        writer.close()
    }
}