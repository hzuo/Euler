package hzuo.euler._022

object Main extends App {

  val text = scalax.io.Resource.fromFile("data/names.txt").string
  val names = text.split(",").map(_.tail.init)
  val ranked = Stream.from(1).zip(names.sorted)

  def alphabetical: Map[Char, Int] = {
    ('A' to 'Z').map(letter => (letter, letter - 'A' + 1))(collection.breakOut)
  }

  def score(rank: Int, name: String): Int = {
    assume(rank >= 1)
    assume(!name.isEmpty)
    rank * name.map(alphabetical).sum
  }

  val answer = ranked.map(Function.tupled(score)).sum
  println(answer)

}