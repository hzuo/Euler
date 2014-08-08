package hzuo.euler._12

object Main extends App {

  def divisors(n: Long) = (1L to scala.math.sqrt(n).toLong).flatMap { x =>
    if (n % x == 0) {
      List(x, n / x)
    } else {
      Nil
    }
  }.toSet

  val nats: Stream[Int] = 1 #:: nats.map(_ + 1)
  val triangulars = nats.scanLeft(0)(_ + _)

  val satisfies = triangulars.filter(divisors(_).size > 500)
  println(satisfies.head)

}