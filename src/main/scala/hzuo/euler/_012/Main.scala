package hzuo.euler._012

import hzuo.euler.Common._

object Main extends App {

  def factorsWithTrivial(x: Long): Set[Long] = {
    if (x == 1L) {
      Set(1L)
    } else {
      factors(x) + 1L + x
    }
  }

  val triangular = Stream.from(1).scanLeft(0)(_ + _).tail
  val answer = triangular.filter(factorsWithTrivial(_).size > 5).head
  println(answer)

}