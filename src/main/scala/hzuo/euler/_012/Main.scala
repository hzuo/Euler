package hzuo.euler._012

import hzuo.euler.Common._

object Main extends App {

  val answer = Stream.from(1).scanLeft(0)(_ + _).filter(factors(_).size > 500).head
  println(answer)

}