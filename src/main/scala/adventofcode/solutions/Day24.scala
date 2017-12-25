package adventofcode.solutions

import adventofcode._

object Day24 extends Day(24) {

  case class Piece(a: Int, b: Int) {
    def strength: Int = a + b
    def contains(port: Int): Boolean = a == port || b == port
  }

  val regex = "([0-9]+)/([0-9]+)".r

  val initial = input.split(lineSeparator).map{ case regex(a, b) => Piece(a.toInt, b.toInt) }.toList

  def maxBridge(port: Int, sum: Int, pieces: List[Piece]): Int = {
    (for {
      i <- pieces.indices
      (left, tail) = pieces.splitAt(i)
      piece = tail.head
      right = tail.tail
      if piece.contains(port)
      otherPort = if (piece.a == port) piece.b else piece.a
      reconstructed: List[Piece] = left ++ right
    } yield maxBridge(otherPort, sum + piece.strength, reconstructed)).toList match {
      case Nil => sum
      case other => other.max
    }
  }

  val partA = maxBridge(0, 0, initial).toString
  solution(partA, A)

  def longestBridge(port: Int, length: Int, sum: Int, pieces: List[Piece]): (Int, Int) = {
    (for {
      i <- pieces.indices
      (left, tail) = pieces.splitAt(i)
      piece = tail.head
      right = tail.tail
      if piece.contains(port)
      otherPort = if (piece.a == port) piece.b else piece.a
      reconstructed: List[Piece] = left ++ right
    } yield longestBridge(otherPort, length + 1, sum + piece.strength, reconstructed)).toList match {
      case Nil => (length, sum)
      case other =>
        val maxLength = other.map(_._1).max
        (maxLength, other.filter(_._1 == maxLength).map(_._2).max)
    }
  }

  val partB = longestBridge(0, 0, 0, initial)._2.toString
  solution(partB, B)
}
