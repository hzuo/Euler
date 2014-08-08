package hzuo.euler._023

import hzuo.euler.Common._

object Main extends App {

  object Type extends Enumeration {
    type Type = Value
    val Deficient, Perfect, Abundant = Value
  }
  import Type._

  def classify(n: Int): Type = {
    val sum = d(n)
    if (sum < n) Deficient
    else if (sum == n) Perfect
    else Abundant
  }

  val target = 1 to 28123
  val table: Map[Int, Type] = target.map(n => (n, classify(n)))(collection.breakOut)
  val isSumOfAbundants =
    locally {
      for {
        i <- target
        if table(i) == Abundant
        j <- target
        if table(j) == Abundant
        sumOfAbundants = i + j
        if sumOfAbundants <= 28123
      } yield sumOfAbundants
    }.toSet

  val answer = target.toStream.filterNot(isSumOfAbundants).sum
  println(answer)

}