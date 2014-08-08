package hzuo.euler

object Common {

  def time[A](block: => A) = {
    val start = System.currentTimeMillis
    block
    println(System.currentTimeMillis - start)
  }

  def fibs(): Iterator[BigInt] = new Iterator[BigInt] {
    var a: BigInt = 0
    var b: BigInt = 0
    override val hasNext = true
    override def next(): BigInt = {
      if (a == 0) {
        a = 1
        1
      } else if (b == 0) {
        b = 1
        1
      } else {
        val tmp = b
        b = a + b
        a = tmp
        b
      }
    }
  }

  def factorial(n: BigInt): BigInt = if (n == 0) 1 else n * factorial(n - 1)

  def streamFrom(n: Long) = Stream.iterate(n)(_ + 1L)

  implicit class Divides(val m: Long) extends AnyVal {
    def divides(n: Long) = n % m == 0L
  }

  def divisors(n: Long): Set[(Long, Long)] = {
    assume(n >= 1)
    val bound = Math.sqrt(n).toLong
    val divisors = for (x <- (2L to bound) if (x divides n)) yield (x, n / x)
    divisors.toSet
  }

  def factors(n: Long): Set[Long] = {
    divisors(n).flatMap { case (a, b) => Set(a, b) }
  }

  def prime(n: Long): Boolean = {
    assume(n > 1)
    divisors(n).isEmpty
  }

  val primes: Stream[Long] = streamFrom(2L).filter(prime)

  // TODO: much repeated work here, see 451 to fix
  def primeFactors(n: Long): Set[Long] = {
    factors(n).filter(prime).toSet
  }

  def d(n: Long): Long = 1L + factors(n).sum

}