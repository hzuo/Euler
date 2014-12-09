package hzuo.euler._034

import hzuo.euler.Common._

object Main extends App {

  val answers = for {
    x <- 3.n to 1000000.n
    if x == x.digits.map(factorial).sum
  } yield x
  println(answers.sum)

}