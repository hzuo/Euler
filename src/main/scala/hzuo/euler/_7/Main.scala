package hzuo.euler._7

import scala.util.control.Breaks._

object Main extends App {

  def factorSpace(n: Long) = (2L to scala.math.sqrt(n).toLong)
  def isPrime(n: Long) = n > 1 && !factorSpace(n).exists(n % _ == 0)

  val ret = {
    var c = 0
    var i = 1
    while (c < 10001) {
      i += 1
      if (isPrime(i)) {
        c += 1
      }
    }
    i
  }
  println(ret)

}