package hzuo.euler._033

import hzuo.euler.Common._

object Main extends App {

  def cancel(a: BigInt, b: BigInt) = {
    val aDigits = a.digits
    val bDigits = b.digits
    val intersection = aDigits.intersect(bDigits)
    for (common <- intersection) yield {
      (aDigits.diff(List(common)).head, bDigits.diff(List(common)).head)
    }
  }

  val answers = for {
    a <- 10.n until 99.n
    b <- (a + 1) to 99.n
    if !(a % 10 == 0 && b % 10 == 0)
    (aPart, bPart) <- cancel(a, b)
    if (a, b).reduced == (aPart, bPart).reduced
  } yield (a, b)
  println(answers.reduceLeft(_ * _).reduced._2)

}