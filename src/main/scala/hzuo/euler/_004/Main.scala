package hzuo.euler._4

object Main extends App {

  def palindromic(n: Int) = {
    val s = n.toString
    s == s.reverse
  }

  val three = (100 to 999)
  val palindromics = for {
    i <- three
    j <- three
    product = i * j
    if palindromic(product)
  } yield product
  println(palindromics.max)

}