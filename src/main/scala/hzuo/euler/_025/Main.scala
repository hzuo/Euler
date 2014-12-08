package hzuo.euler._025

import hzuo.euler.Common._

object Main extends App {

  val threshold = BigInt(10).pow(999)
  val answer = fibs().indexWhere(_ >= threshold)
  println(answer)

}