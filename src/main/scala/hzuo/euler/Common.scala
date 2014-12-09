package hzuo.euler

object Common {

  def time[A](block: => A) = {
    val start = System.currentTimeMillis
    block
    println(System.currentTimeMillis - start)
  }

  def memo[A, B](f: A => B): A => B = {
    val cache = collection.mutable.Map.empty[A, B]
    (a: A) => cache.getOrElseUpdate(a, f(a))
  }

  def memoRecursive[A, B](f: (A => B) => (A => B)): A => B = {
    val cache = collection.mutable.Map.empty[A, B]
    def caching(x: A): B = cache.getOrElseUpdate(x, f(caching)(x))
    caching
  }

  def fibs(): Iterator[BigInt] = new Iterator[BigInt] {
    var i: BigInt = 0
    var a: BigInt = 0
    var b: BigInt = 1
    override val hasNext = true
    override def next(): BigInt = {
      if (i == 0) {
        i += 1
        0
      } else if (i == 1) {
        i += 1
        1
      } else {
        val tmp = b
        b = a + b
        a = tmp
        b
      }
    }
  }

  // no overlapping subproblems, no need for memoRecursive
  val factorial: BigInt => BigInt = memo { (n: BigInt) =>
    if (n == 0) 1 else n * factorial(n - 1)
  }

  def streamFrom(n: Long) = Stream.iterate(n)(_ + 1L)

  implicit class MyLong(val m: Long) extends AnyVal {
    def divides(n: Long) = n % m == 0L
  }

  // TODO: range-based memoization
  val divisors: Long => Set[(Long, Long)] = memo { (n: Long) =>
    assume(n >= 1)
    val bound = Math.sqrt(n).toLong
    val divisors = for (x <- (2L to bound) if (x divides n)) yield (x, n / x)
    divisors.toSet
  }

  val factors: Long => Set[Long] = memo { (n: Long) =>
    divisors(n).flatMap { case (a, b) => Set(a, b) }
  }

  def prime(n: Long): Boolean = {
    assume(n > 1)
    divisors(n).isEmpty
  }

  val primes: Stream[Long] = streamFrom(2L).filter(prime)

  val primeFactors: Long => Set[Long] = memo { (n: Long) =>
    factors(n).filter(prime).toSet
  }

  val d: Long => Long = memo { (n: Long) =>
    1L + factors(n).sum
  }

  def gcd(n: BigInt, m: BigInt): BigInt = {
    if (m > n) {
      gcd(m, n)
    } else if (m == 0) {
      n
    } else {
      gcd(m, n % m)
    }
  }

  implicit class MyInt(val x: Int) extends AnyVal {
    def n: BigInt = BigInt(x)
  }

  implicit class MyString(x: String) {
    def n: BigInt = BigInt(x)
  }

  implicit class MyBigInt(x: BigInt) {
    def digits: Seq[BigInt] = x.toString.map(_.toString.n)
  }

  type Fraction = (BigInt, BigInt)

  implicit class RichFraction(a: Fraction) {
    def reduced: Fraction = a match {
      case (n, d) =>
        val factor = gcd(n, d)
        (n / factor, d / factor)
    }
    def +(b: Fraction): Fraction = (a, b) match {
      case ((c, d), (e, f)) =>
        ((c * f + e * d), (d * f))
    }
    def *(b: Fraction): Fraction = (a, b) match {
      case ((c, d), (e, f)) =>
        (c * e, d * f)
    }
  }

}