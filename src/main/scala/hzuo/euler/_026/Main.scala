package hzuo.euler._026

object Main extends App {

  type Fraction = (BigInt, BigInt)

  def gcd(n: BigInt, m: BigInt): BigInt = {
    if (m > n) {
      gcd(m, n)
    } else if (m == 0) {
      m
    } else {
      gcd(m, n % m)
    }
  }

  def reduce(f: Fraction): Fraction = f match {
    case (n, d) =>
      val factor = gcd(n, d)
      (n / factor, d / factor)
  }

  def isZeroPeriod(f: Fraction) = {
    val (n, d) = reduce(f)
    d == 1 || d % 2 == 0 || d % 5 == 0
  }

  val _10pow: Stream[BigInt] = BigInt(2) #:: BigInt(10) #:: _10pow.tail.map(_ * 10)
  def period(d: Int) = {
    _10pow.indexWhere { n =>
      isZeroPeriod((n - 1, d))
    }
  }

  println((2 until 1000).map(x => (x, period(x))).maxBy(_._2)._1)

}