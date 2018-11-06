package scalaam

object Main {
  def main(args: Array[String]) = {
    ()
  }

  def lambda() = {
    import scalaam.core._
    import scalaam.graph._
    import scalaam.language.lambda._
    import scalaam.machine._

    val address   = NameAddress
    val timestamp = ZeroCFA[LambdaExp]()
    val lattice   = LambdaSetLattice[address.A]()
    val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](
      address.Alloc[timestamp.T, LambdaExp])
    val machine = new AAM[LambdaExp, address.A, lattice.L, timestamp.T](sem)
    val graph   = DotGraph[machine.State, machine.Transition]
    val result = machine.run[graph.G](
      LambdaParser.parse("((lambda (x) (lambda (y) y)) (lambda (z) z))"),
      Timeout.Infinity)
    result.toFile("foo.dot")
  }
}


/* To be used with the console: `sbt console`, then scalaam.SchemeRun.run(file) */
object SchemeRun {
  import scalaam.core._
  import scalaam.graph._
  import scalaam.language.scheme._
  import scalaam.lattice._
  import scalaam.machine._

  val address   = NameAddress
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
  val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = DotGraph[machine.State, machine.Transition]

  def run(file: String, timeout: Timeout.T = Timeout.seconds(10)) = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    val t0 = System.nanoTime
    val result = machine.run[graph.G](
      SchemeParser.parse(content),
      timeout)
    val t1 = System.nanoTime
    if (timeout.reached) { println("Time out!") } else { println(s"Time: ${(t1 - t0) / 1000000}ms") }
    f.close()
    result.toFile("foo.dot")
    import Graph.GraphOps
    println(s"States: ${result.nodes}")
    result
  }
}

/* To be used with the console: `sbt console`, then scalaam.AtomlangRun.run(file) */
object AtomlangRun {
  import scalaam.core._
  import scalaam.graph._
  import scalaam.language.atomlang._
  import scalaam.language.scheme._
  import scalaam.lattice._
  import scalaam.machine._
  
  val address   = NameAddress
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
  val machine = new AAM[SchemeExp, address.A, lattice.L, timestamp.T](sem)
  val graph   = DotGraph[machine.State, machine.Transition]
  
  def run(file: String, out: String = "foo.dot", timeout: Timeout.T = Timeout.seconds(10)): AtomlangRun.graph.G = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    val t0 = System.nanoTime
    val result = machine.run[graph.G](
      SchemeParser.parse(content),
      timeout)
    val t1 = System.nanoTime
    if (timeout.reached) { println("Time out!") } else { println(s"Time: ${(t1 - t0) / 1000000}ms") }
    f.close()
    result.toFile(out)
    import Graph.GraphOps
    println(s"States: ${result.nodes}")
    result
  }
}
