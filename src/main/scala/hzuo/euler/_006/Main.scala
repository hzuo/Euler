package hzuo.euler._6

object Main extends App {

  val xs = (1 to 100)
  val sum = xs.sum
  val squares = xs.map(x => x * x)
  println(sum * sum - squares.sum)

}