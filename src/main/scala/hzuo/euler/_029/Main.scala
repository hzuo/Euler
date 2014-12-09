package hzuo.euler._029

object Main extends App {

  val values = for {
    a <- BigInt(2) to BigInt(100)
    b <- 2 to 100
  } yield a.pow(b)
  println(values.toSet.size)

}