package hzuo.euler._96

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.util._

object Sudoku {

  type Square = (Int, Int)
  type State = Either[Set[Int], Int]
  type Board = Map[Square, State]
  type Rule = (Board, Square) => Board
  type Puzzle = Map[Square, Int]

  case object Contradiction extends Exception

  val squares = for { x <- 1 to 9; y <- 1 to 9 } yield (x, y)
  val candidates = (1 to 9).toSet

  @tailrec
  def deduce(rules: Seq[Rule], board: Board): Board = {
    val rulesApplied = rules.foldLeft(board) {
      (board, rule) => squares.foldLeft(board)(rule)
    }
    if (board == rulesApplied) {
      board
    } else {
      deduce(rules, rulesApplied)
    }
  }

  def completed(board: Board) = board.forall(_._2.isRight)

  def solutions(rules: Seq[Rule], board: Board): Iterable[Board] = {
    val deduction = deduce(rules, board)
    if (completed(deduction)) Some(deduction)
    else {
      val target = deduction.minBy {
        case (square, state) =>
          state match {
            case Right(_) => Integer.MAX_VALUE
            case Left(xs) => xs.size
          }
      }
      val (square, cs) = (target._1, target._2.left.get)
      val randomized = Random.shuffle(cs)
      val ret = for (c <- randomized) yield {
        val guessed = deduction.updated(square, Right(c))
        Try(solutions(rules, guessed)).toOption
      }
      ret.flatten.flatten
    }
  }

  def solve(board: Puzzle): Iterable[Puzzle] = {
    val parsed: Board = squares.map { square =>
      val state = board.get(square) match {
        case None => Left(candidates)
        case Some(x) => Right(x)
      }
      (square, state)
    }(breakOut)
    val mySolutions = solutions(NormalSudoku.Rules, parsed)
    mySolutions.map { solution =>
      solution.map { case (square, d) => (square, d.right.get) }
    }(breakOut)
  }

}