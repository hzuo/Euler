package hzuo.euler._007

import scala.util.control.Breaks._

import hzuo.euler.Common._

object Main extends App {

  val answer = Stream.from(1).zip(primes).filter(_._1 == 10001).head._2
  println(answer)

}