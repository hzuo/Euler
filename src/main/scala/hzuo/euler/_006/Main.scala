package hzuo.euler._006

object Main extends App {

  val xs = (1 to 100)
  val sum = xs.sum
  val squares = xs.map(x => x * x)
  val answer = sum * sum - squares.sum
  println(answer)

}