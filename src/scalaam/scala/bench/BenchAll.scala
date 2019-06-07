package scalaam.bench

import scalaam.BenchTime
import scalaam.bench.BenchConfig._

import scala.bench.{BenchReuse, BenchSoundness}

object BenchAll extends App {
    display("Benchmarking soundness.")
    BenchSoundness.main(Array())
    display("Benchmarking analysis time.")
    BenchTime.main(Array())
    display("Benchmarking reuse.")
    BenchReuse.main(Array())
}
