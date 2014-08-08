package hzuo.euler._16

object Main extends App {

  val answer = BigInt(2).pow(1000).toString.map(_.toString.toInt).sum
  println(answer)

}