package hzuo.euler._96

import Sudoku._

object Main extends App {

  def createPuzzle(s: String): Puzzle = {
    val lines = scala.io.Source.fromString(s).getLines
    val parsed = for {
      (line, row) <- lines.zipWithIndex
      (x, column) <- line.zipWithIndex
    } yield {
      x match {
        case '0' => None
        case _ => Some((row + 1, column + 1), x.toString.toInt)
      }
    }
    parsed.flatten.toMap
  }

  def showPuzzle(board: Puzzle): String = {
    val rows =
      for (row <- 1 to 9) yield {
        for (col <- 1 to 9) yield {
          board.get(row, col).getOrElse(0)
        }
      }
    val rowsConcatenated = rows.map(_.map(_.toString).reduceLeft(_ + _))
    rowsConcatenated.mkString("\n")
  }

  val contents = scala.io.Source.fromFile("data/sudoku.txt").mkString

  val puzzles = for {
    piece <- contents.split("""Grid (\d\d)""")
    trimmed = piece.trim
    if !trimmed.isEmpty
  } yield createPuzzle(trimmed)

  def extract(puzzle: Puzzle) = {
    val solved = solve(puzzle).head
    solved(1, 1) * 100 + solved(1, 2) * 10 + solved(1, 3)
  }

  val puzzlesPar = puzzles.par
  val answer = puzzlesPar.aggregate(0)(_ + extract(_), _ + _)
  println(answer)

}