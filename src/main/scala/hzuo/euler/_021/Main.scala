package hzuo.euler._021

import hzuo.euler.Common._

object Main extends App {

  // fixpoints of d compose d with an additional requirement
  def amicable(a: Long) = {
    val b = d(a)
    a != b && d(b) == a
  }

  val answer = streamFrom(1L).takeWhile(_ < 10000L).filter(amicable).sum
  println(answer)

}