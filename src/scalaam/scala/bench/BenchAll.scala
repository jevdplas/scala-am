package scalaam.bench

import scalaam.bench.BenchConfig._
import scalaam.bench.{BenchReuse, BenchTime, BenchSoundness}

object BenchAll extends App {
  // display("Benchmarking soundness.")
  // BenchSoundness.main(Array())
  display("Benchmarking reuse.")
  BenchReuse.main(Array())
  display("Benchmarking analysis time.")
  BenchTime.main(Array())
}
