package hzuo.euler._14

object Main extends App {

  //  def Y[A, B](f: (A => B) => (A => B))(x: A): B = f(Y(f))(x)

  def memo[A, B](f: (A => B) => (A => B)): A => B = {
    val cache = collection.mutable.Map.empty[A, B]
    def caching(x: A): B = cache.getOrElseUpdate(x, f(caching)(x))
    caching
  }

  val collatzLength = {
    def f(self: Long => Long)(n: Long): Long = {
      if (n == 1) 1
      else if (n % 2 == 0) 1 + self(n / 2)
      else 1 + self(3 * n + 1)
    }
    memo(f)
  }

  val lengths = for (i <- 1 until 1000000) yield (collatzLength(i), i)
  println(lengths.max._2)

}