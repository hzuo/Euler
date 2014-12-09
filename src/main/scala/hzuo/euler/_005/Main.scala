package hzuo.euler._005

import hzuo.euler.Common._

object Main extends App {

  def criteria(x: Int) = (2 to 20).forall(x % _ == 0)
  val answer = Stream.from(21).filter(criteria).head
  println(answer)

}