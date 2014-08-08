package hzuo.euler._024

object Main extends App {

  val answer = (0 to 9).toSeq.permutations.toIndexedSeq(1000000 - 1)
  println(answer.mkString)

}