package hzuo.euler._027

import hzuo.euler.Common._

object Main extends App {

  def eval(a: Int, b: Int)(n: Int) = n * n + a * n + b

  def countConsecutivePrimes(a: Int, b: Int) = {
    val e: Int => Int = eval(a, b)
    Stream.from(0).takeWhile { x =>
      val y = e(x)
      y > 1 && isPrime(y)
    }.length
  }

  val space = for {
    a <- -999 to 999
    b <- -999 to 999
  } yield ((a, b), countConsecutivePrimes(a, b))
  val (a, b) = space.maxBy(_._2)._1
  println(a * b)

}