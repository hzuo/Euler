package hzuo.euler._032

import hzuo.euler.Common._

object Main extends App {

  val identities = for {
    // possible scala bug here?
    // permutations iterator allegedly distinct?
    digits <- (1 to 9).permutations.toSet[IndexedSeq[Int]]
    split <- 1 to 2
    a = concat(digits.slice(0, split))
    b = concat(digits.slice(split, 5))
    c = concat(digits.drop(5))
    if a * b == c
  } yield (a, b, c)
  println(identities.map(_._3).sum)

}