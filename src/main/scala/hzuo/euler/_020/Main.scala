package hzuo.euler._20

object Main extends App {

  def fact(n: BigInt): BigInt = if (n == 0) 1 else n * fact(n - 1)

  println(fact(100).toString.map(_.toString.toInt).sum)

}