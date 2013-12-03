package hzuo.euler._96

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.util._

object Sudoku {

  private[this] object Private {

    type Square = (Int, Int)
    type Component = Iterable[Square]
    type State = Either[Set[Int], Int]
    type Board = Map[Square, State]
    type Rule = (Board, Square) => Board

    case object Contradiction extends Exception

    val squares = for { x <- 1 to 9; y <- 1 to 9 } yield (x, y)
    val candidates = (1 to 9).toSet

    def invertedIndex[A](xss: Iterable[Iterable[A]]): Map[A, Iterable[A]] = {
      (for (xs <- xss; x <- xs) yield (x, xs))(scala.collection.breakOut)
    }

    def |*[T](ss: Iterable[Set[T]]) = ss.fold(Set.empty[T])(_ | _)

    def createRule(components: Iterable[Iterable[Component]]): Rule = {
      val invertedIndices = components.map(invertedIndex)
      val lookupPeers: Map[Square, Component] = squares.map { square =>
        val sets = invertedIndices.map(invertedIndex => invertedIndex(square)).map(_.toSet)
        (square, |*(sets))
      }(breakOut)
      (board, square) => board(square) match {
        case Right(d) => board
        case Left(cs) =>
          val knownForPeers = lookupPeers(square).flatMap(x => board(x).right.toOption).toSet
          val possibleForMe = cs &~ knownForPeers
          possibleForMe.size match {
            case 0 => throw Contradiction
            case 1 => board.updated(square, Right(possibleForMe.head))
            case _ => board.updated(square, Left(possibleForMe))
          }
      }
    }

    val normalRule = createRule {
      val rows: Iterable[Component] =
        for { x <- 1 to 9 } yield {
          for { y <- 1 to 9 } yield (x, y)
        }
      val columns: Iterable[Component] =
        for { y <- 1 to 9 } yield {
          for { x <- 1 to 9 } yield (x, y)
        }
      val blocks: Iterable[Component] =
        for { xOff <- List(0, 3, 6); yOff <- List(0, 3, 6) } yield {
          for { x <- 1 to 3; y <- 1 to 3 } yield (x + xOff, y + yOff)
        }
      List(rows, columns, blocks)
    }

    // should use state monad
    @tailrec
    def deduce(rule: Rule, board: Board): Board = {
      val applied = squares.foldLeft(board)(rule)
      if (board == applied) {
        board
      } else {
        deduce(rule, applied)
      }
    }

    def completed(board: Board) = board.forall(_._2.isRight)

    def solutions(rule: Rule, board: Board): Iterable[Board] = {
      val deduction = deduce(rule, board)
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
          Try(solutions(rule, guessed)).toOption
        }
        ret.flatten.flatten
      }
    }

  }

  import Private._

  type Puzzle = Map[(Int, Int), Int]

  def solve(board: Puzzle): Iterable[Puzzle] = {
    val parsed: Board = squares.map { square =>
      val state = board.get(square) match {
        case None => Left(candidates)
        case Some(x) => Right(x)
      }
      (square, state)
    }(breakOut)
    val mySolutions = solutions(normalRule, parsed)
    mySolutions.map { solution =>
      solution.map { case (square, d) => (square, d.right.get) }
    }(breakOut)
  }

}