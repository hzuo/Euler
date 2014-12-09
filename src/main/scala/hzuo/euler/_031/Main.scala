package hzuo.euler._031

import hzuo.euler.Common._

object Main extends App {

  def decompose(denominations: List[Int], amount: Int): List[Map[Int, Int]] = {
    if (amount == 0) List(Map())
    else if (amount < 0 || denominations.isEmpty) List()
    else {
      val d = denominations.head
      val maxCanFit = amount / d
      for {
        n <- (0 to maxCanFit).toList
        remaining = amount - (d * n)
        decomposed <- decompose(denominations.tail, amount - (d * n))
      } yield if (n == 0) decomposed else decomposed + (d -> n)
    }
  }

  val denominations = List(1, 2, 5, 10, 20, 50, 100, 200)
  println(decompose(denominations, 200).size)

}