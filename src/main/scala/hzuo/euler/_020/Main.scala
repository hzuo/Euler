package hzuo.euler._020

import hzuo.euler.Common._

object Main extends App {

  val answer = factorial(100).toString.map(_.toString.toInt).sum
  println(answer)

}