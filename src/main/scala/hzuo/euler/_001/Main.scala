package hzuo.euler._001

import hzuo.euler.Common._

object Main extends App {

  val answer = (1 until 1000).filter(x => (3.n divides x) || (5.n divides x)).sum
  println(answer)

}