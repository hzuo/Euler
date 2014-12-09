package hzuo.euler._032

object Main extends App {

  def concat(xs: Seq[Int]): Int = {
    xs.foldLeft("")(_ + _).toInt
  }

  val identities = for {
    digits <- (1 to 9).permutations
    split <- 1 to 2
    a = concat(digits.slice(0, split))
    b = concat(digits.slice(split, 5))
    c = concat(digits.drop(5))
    if a * b == c
  } yield (a, b, c)
  println(identities.map(_._3).sum)

}