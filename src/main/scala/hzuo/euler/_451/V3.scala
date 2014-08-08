package hzuo.euler._451

object V3 extends App {

  def extendedEuclideanAlgorithm(m: Int, n: Int): (Int, Int, Int) = {
    if (n == 0) {
      (m, 1, 0)
    } else {
      val (q, r) = (m / n, m % n)
      val (g, s, t) = extendedEuclideanAlgorithm(n, r)
      (g, t, s - t * q)
    }
  }

  def multiplicativeInverse(a: Int, m: Int): Option[Int] = {
    val (g, s, t) = extendedEuclideanAlgorithm(a, m)
    if (g == 1) {
      val add = if (s < 0) m else 0
      Some(s + add)
    } else {
      None
    }
  }

  def I(m: Int): Int = {
    (m - 2 to 1 by -1).find { i =>
      multiplicativeInverse(i, m) match {
        case Some(inv) if i == inv => true
        case _ => false
      }
    }.get
  }

  val answer = (3 to 20000000).par.aggregate(0)(_ + I(_), _ + _)
  println(answer)

}