package hzuo.euler._451

import scala.collection.immutable.SortedSet

import hzuo.euler.Common._

object V1 extends App {

  def coprime(n: Long) = {
    (m: Long) => !primeFactors(n).exists(_ divides m)
  }

  def I(x: Long): Long = {
    val coprimeToX = coprime(x)
    (x - 2 to 1 by -1).find { i =>
      coprimeToX(i) && (i * i) % x == 1
    }.get
  }

  val answer = (3L to 20000000L).par.aggregate(0L)(_ + I(_), _ + _)
  println(answer)

}