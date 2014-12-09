package hzuo.euler._026

import hzuo.euler.Common._

object Main extends App {

  def isZeroPeriod(f: Fraction) = {
    val (n, d) = f.reduced
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