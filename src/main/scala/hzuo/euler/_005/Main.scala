package hzuo.euler._5

import scala.util.control.Breaks._

object Main extends App {

  def criteria(n: Int) = (2 to 20).forall(n % _ == 0)
  val accepted = {
    var candidate = 21
    breakable {
      while (true) {
        if (criteria(candidate)) break
        else candidate += 1
      }
    }
    candidate
  }
  println(accepted)

}