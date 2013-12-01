package hzuo.euler._96

import scala.collection.breakOut
import Sudoku._

object NormalSudoku {

  def invertedIndex[A](xss: Iterable[Iterable[A]]): Map[A, Iterable[A]] = {
    (for (xs <- xss; x <- xs) yield (x, xs))(scala.collection.breakOut)
  }

  type Component = Iterable[Square]

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
  val lookupRow = invertedIndex(rows)
  val lookupColumn = invertedIndex(columns)
  val lookupBlock = invertedIndex(blocks)

  val lookupPeers: Map[Square, Component] = squares.map { square =>
    val peers = lookupRow(square).toSet | lookupColumn(square).toSet | lookupBlock(square).toSet
    (square, peers - square)
  }(breakOut)

  val normal: Rule = (board, square) => board(square) match {
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

  val Rules = Seq(normal)

}