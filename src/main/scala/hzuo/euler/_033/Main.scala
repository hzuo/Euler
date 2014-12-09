package hzuo.euler._033

import hzuo.euler.Common._

object Main extends App {

  def cancel(a: Int, b: Int) = {
    val aDigits = List(a / 10, a % 10)
    val bDigits = List(b / 10, b % 10)
    val intersection = aDigits.intersect(bDigits)
    for (common <- intersection) yield {
      (aDigits.diff(List(common)).head, bDigits.diff(List(common)).head)
    }
  }

  val answers = for {
    a <- 10 until 99
    b <- (a + 1) to 99
    if !(a % 10 == 0 && b % 10 == 0)
    (aPart, bPart) <- cancel(a, b)
    if (a, b).toFraction.reduced == (aPart, bPart).toFraction.reduced
  } yield (a, b).toFraction
  println(answers.reduceLeft(_ * _).reduced._2)

}