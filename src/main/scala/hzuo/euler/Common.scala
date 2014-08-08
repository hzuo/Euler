package hzuo.euler

object Common {

  def time[A](block: => A) = {
    val start = System.currentTimeMillis
    block
    println(System.currentTimeMillis - start)
  }

  val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  def factorial(n: BigInt): BigInt = if (n == 0) 1 else n * factorial(n - 1)

  def streamFrom(n: Long) = Stream.iterate(n)(_ + 1L)

  implicit class Divides(val m: Long) extends AnyVal {
    def divides(n: Long) = m % n == 0
  }

  def divisors(n: Long): Set[(Long, Long)] = {
    if (n <= 0) throw new IllegalArgumentException
    val bound = Math.sqrt(n).toLong
    val divisors = for (x <- (2L to bound) if (x divides n)) yield (x, n / x)
    divisors.toSet
  }

  def factors(n: Long): Set[Long] = {
    divisors(n).flatMap { case (a, b) => Set(a, b) }
  }

  def prime(n: Long): Boolean = {
    if (n <= 1) throw new IllegalArgumentException
    else divisors(n).isEmpty
  }

  val primes: Stream[Long] = streamFrom(2L).filter(prime)

  // TODO: much repeated work here, see 451 to fix
  def primeFactors(n: Long): Set[Long] = {
    factors(n).filter(prime).toSet
  }

}