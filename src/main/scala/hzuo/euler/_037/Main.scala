package hzuo.euler._037

import hzuo.euler.Common._

object Main extends App {

  def isTruncatable(x: Long) = {
    def test(digits: IndexedSeq[Int], f: IndexedSeq[Int] => IndexedSeq[Int]): Boolean = {
      var curr = digits
      while (!curr.isEmpty) {
        if (!isPrime(concat(curr).toLong)) {
          return false
        }
        curr = f(curr)
      }
      return true
    }
    val digits = x.n.digits
    test(digits, _.tail) && test(digits, _.init)
  }

  println(primes.dropWhile(_ < 10).filter(isTruncatable).take(11).sum)

}