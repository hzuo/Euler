package hzuo.euler._451

import scala.collection.immutable.SortedSet

object V2 extends App {

  def primeFactorTable(bound: Int): Map[Int, List[Int]] = {
    val primes = collection.mutable.ArrayBuffer.empty[Int]
    val primeFactorTable = collection.mutable.Map.empty[Int, List[Int]]
    for (i <- 2 to (bound / 2)) {
      val primeFactors = primeFactorTable.get(i).getOrElse {
        primes += i
        List(i)
      }
      import scala.util.control.Breaks._
      breakable {
        for (prime <- primes) {
          val compositeDiscovered = prime * i
          if (compositeDiscovered > bound) break
          primeFactorTable(compositeDiscovered) = prime :: primeFactors
        }
      }
    }
    primeFactorTable.toMap
  }

  hzuo.euler.Common.time {
    primeFactorTable(20000000)
  }

}