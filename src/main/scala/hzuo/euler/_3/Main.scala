package hzuo.euler._3

object Main extends App {

  def factorSpace(n: Long) = (2L to scala.math.sqrt(n).toLong)
  def isPrime(n: Long) = !factorSpace(n).exists(n % _ == 0)

  val n = 600851475143L
  val factors = factorSpace(n).flatMap { x =>
    if (n % x == 0) {
      List(x, n / x)
    } else {
      Nil
    }
  }
  println(factors.filter(isPrime).max)

}