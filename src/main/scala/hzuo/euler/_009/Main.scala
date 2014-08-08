package hzuo.euler._009

object Main extends App {

  def pythagoreanTriple(a: Int, b: Int, c: Int) = a * a + b * b == c * c
  def solutions(n: Int, components: Int): List[List[Int]] = {
    if (components == 1) List(List(n))
    else {
      for {
        fixed <- 0 to n
        free <- solutions(n - fixed, components - 1)
      } yield fixed :: free
    }.toList
  }

  val answer = for {
    solution <- solutions(1000, 3)
    if solution.forall(_ > 0)
    a :: b :: c :: Nil = solution
    if pythagoreanTriple(a, b, c)
  } yield a * b * c
  println(answer)

}