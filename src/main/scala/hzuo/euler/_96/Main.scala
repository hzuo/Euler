package hzuo.euler._96

import Sudoku._

object Main extends App {

  def parse(s: String): Puzzle = {
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

  def showBoard(board: Puzzle): String = {
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
  } yield parse(trimmed)

  hzuo.euler.util.time {
    var sum = 0
    for (puzzle <- puzzles) {
      val solved = solve(puzzle).head
      val three = solved(1, 1) * 100 + solved(1, 2) * 10 + solved(1, 3)
      sum += three
    }
    println(sum)
  }

}