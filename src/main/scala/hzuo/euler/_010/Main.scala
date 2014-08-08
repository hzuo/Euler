package hzuo.euler._10

import hzuo.euler.Common._

object Main extends App {

  val answer = primes.takeWhile(_ < 2000000L).sum
  println(answer)

}