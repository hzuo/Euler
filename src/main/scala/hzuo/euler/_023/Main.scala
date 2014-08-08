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

  val limit = 28123
  val universe = 1 to limit
  val isSumOfAbundants =
    locally {
      for {
        i <- universe
        if classify(i) == Abundant
        j <- universe
        if classify(j) == Abundant
        sumOfAbundants = i + j
        if sumOfAbundants <= limit
      } yield sumOfAbundants
    }.toSet

  val answer = universe.filterNot(isSumOfAbundants).sum
  println(answer)

}