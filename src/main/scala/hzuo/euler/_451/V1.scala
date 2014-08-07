package hzuo.euler._451

import scala.collection.immutable.SortedSet

object V1 extends App {

  def factorSpace(n: Long) = (2L to scala.math.sqrt(n).toLong)
  def isPrime(n: Long) = n > 1 && !factorSpace(n).exists(n % _ == 0)
  def factors(n: Long) = factorSpace(n).flatMap { x =>
    if (n % x == 0) {
      List(x, n / x)
    } else {
      Nil
    }
  }

  def coprime(n: Long) = {
    val primeFactors = factors(n).filter(isPrime)
    (m: Long) => !primeFactors.exists(m % _ == 0)
  }

  def I(x: Long): Long = {
    val coprimeToX = coprime(x)
    (x - 2 to 1 by -1).find { i =>
      coprimeToX(i) && (i * i) % x == 1
    }.get
  }

  val ret = (3L to 20000000L).par.aggregate(0L)(_ + I(_), _ + _)
  println(ret)

}