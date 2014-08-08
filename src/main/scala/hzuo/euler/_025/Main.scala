package hzuo.euler._025

import hzuo.euler.Common._

object Main extends App {

  val answer = fibs().indexWhere(_ >= BigInt(10).pow(999)) + 1
  println(answer)

}