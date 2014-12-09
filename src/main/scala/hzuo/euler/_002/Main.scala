package hzuo.euler._002

import hzuo.euler.Common._

object Main extends App {

  val answer = fibs().map(_.toInt).takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum
  println(answer)

}