package hzuo.euler._017

object Main extends App {

  val en = NumeralRead(Languages.en)
  def countLetters(n: Int) = {
    val addAnd = if (n > 100 && n % 100 != 0) 3 else 0
    en(n).filter(_.isLetter).length + addAnd
  }

  val answer = (1 to 1000).map(countLetters).sum
  println(answer)

}