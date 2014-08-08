package hzuo.euler._002

import hzuo.euler.Common._

object Main extends App {

  val answer = fibs().takeWhile(_ <= 4000000).filter(2 divides _.toLong).sum
  println(answer)

}