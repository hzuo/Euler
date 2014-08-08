package hzuo.euler._067

import hzuo.euler.Common._

object Main extends App {

  val lines = scala.io.Source
    .fromFile("data/triangle.txt")
    .getLines.toArray

  val parsed =
    for {
      line <- lines
    } yield {
      for {
        word <- line.split("""\s""")
      } yield word.toInt
    }

  def max(ns: Array[Array[Int]]) = {
    val anchored = {
      def f(self: ((Int, Int)) => Int)(position: (Int, Int)) = {
        val (r, c) = position
        val me = ns(r)(c)
        if (r == 0 && c == 0) {
          me
        } else {
          val parentMaxes = for {
            (r, c) <- List((r - 1, c - 1), (r - 1, c))
            if 0 <= c && c <= r
            parentMax = self(r, c)
          } yield parentMax
          parentMaxes.max + me
        }
      }
      memoRecursive(f)
    }
    val lastRow = ns.length - 1
    (for (c <- 0 to lastRow) yield anchored(lastRow, c)).max
  }

  println(max(parsed))

}