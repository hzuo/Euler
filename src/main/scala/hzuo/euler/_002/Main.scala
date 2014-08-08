package hzuo.euler._2

import hzuo.euler.Common._

object Main extends App {

  val answer = fibs.takeWhile(_ <= 4000000).filter(_ divides 2).sum
  println(answer)

}