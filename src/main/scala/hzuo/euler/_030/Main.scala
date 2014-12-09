package hzuo.euler._030

object Main extends App {

  def pow5(i: Int): Int = {
    val _2 = i * i
    val _4 = _2 * _2
    _4 * i
  }

  val answers = {
    for {
      n <- 2 to 1000000
      sum = n.toString.map(digit => pow5(digit.toString.toInt)).sum
      if sum == n
    } yield n
  }
  println(answers.sum)

}