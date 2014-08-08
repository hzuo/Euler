package hzuo.euler._1

import hzuo.euler.Common._

object Main extends App {

  val answer = (1 until 1000).filter(x => (3 divides x) || (5 divides x)).sum
  println(answer)

}