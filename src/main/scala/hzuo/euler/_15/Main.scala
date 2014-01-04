package hzuo.euler._15

object Main extends App {

  def factorial(n: BigInt): BigInt = if (n == 0) 1 else n * factorial(n - 1)

  val ret = factorial(20 + 20) / (factorial(20) * factorial(20))
  println(ret)

}