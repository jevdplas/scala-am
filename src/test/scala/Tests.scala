import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{Matchers, PropSpec}
import scalaam.core._
import scalaam.graph.Graph._
import scalaam.graph.{GraphMetadataBool, GraphMetadataString, GraphMetadataValue, ReachableStatesConditionGraph}
import scalaam.language.scheme.{SchemeExp, SchemeLattice, SchemeParser}

abstract class Tests[A <: Address, V, T, C](
                                               implicit val timestamp: Timestamp[T, C],
                                               implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
    val sem: Semantics[SchemeExp, A, V, T, C]
    val machine: MachineAbstraction[SchemeExp, A, V, T, C]
    val graph = ReachableStatesConditionGraph[machine.State, machine.Transition](n => n.metadata.find("halted") == Some(GraphMetadataBool(true)))
    
    def checkResult(program: String, answer: V, timeout: Timeout.T = Timeout.Infinity) = {
        val result = machine.run[graph.G](SchemeParser.parse(program), timeout)
        if (timeout.reached) {
            cancel(s"time out")
        } else {
            val resultVals = result.findNodes(n => n.metadata.find("type") == Some(GraphMetadataString("kont"))).flatMap[V, Set[V]]({ n => n.metadata.find("value") match {
                case Some(GraphMetadataValue(v : V @unchecked)) => Set(v)
                case _ => Set()
            }})
            assert(!resultVals.find(v => lat.subsumes(v, answer)).isEmpty)
        }
    }
    def check(table: TableFor2[String, V]) =
        forAll (table) { (program: String, answer: V) =>
            checkResult(program, answer)
        }
}
