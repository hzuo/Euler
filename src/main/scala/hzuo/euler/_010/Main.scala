package hzuo.euler._10

object Main extends App {

  def factorSpace(n: Long) = (2L to scala.math.sqrt(n).toLong)
  def isPrime(n: Long) = n > 1 && !factorSpace(n).exists(n % _ == 0)

  println((2L until 2000000L).filter(isPrime).sum)

}