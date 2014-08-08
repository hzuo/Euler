package hzuo.euler._010

import hzuo.euler.Common._

object Main extends App {

  val answer = primes.takeWhile(_ < 2000000L).sum
  println(answer)

}