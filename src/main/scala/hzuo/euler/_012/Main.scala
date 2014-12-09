package hzuo.euler._012

import hzuo.euler.Common._

object Main extends App {

  def factorsWithTrivial(x: Long): Set[Long] = {
    if (x == 1.n) {
      Set(1)
    } else {
      factors(x) + 1 + x
    }
  }

  val triangular = Stream.from(1).scanLeft(0)(_ + _).tail
  val answer = triangular.filter(factorsWithTrivial(_).size > 500).head
  println(answer)

}