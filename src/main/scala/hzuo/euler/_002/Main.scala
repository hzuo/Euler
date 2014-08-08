package hzuo.euler._002

import hzuo.euler.Common._

object Main extends App {

  val answer = fibs().map(_.toInt).takeWhile(_ <= 4000000).filter(2 divides _).sum
  println(answer)

}