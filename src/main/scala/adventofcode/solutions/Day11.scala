package adventofcode.solutions

import adventofcode._

object Day11 extends Day(11) {

  type Position = (Int, Int, Int)

  val list = input.split(",").toList

  def distance(position: Position): Int = {
    val (x, y, z) = position
    (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2
  }

  def move(list: List[String], position: Position, max: Int): (Position, Int) = {
    val (x, y, z) = position
    list match {
      case Nil => (position, max)
      case a :: as =>
        val next = a match {
          case "n" => (x, y + 1, z - 1)
          case "ne" => (x + 1, y, z - 1)
          case "se" => (x + 1, y - 1, z)
          case "s" => (x, y - 1, z + 1)
          case "sw" => (x - 1, y, z + 1)
          case "nw" => (x - 1, y + 1, z)
        }
        move(as, next, Math.max(distance(next), max))
    }
  }

  val solution = move(list, (0, 0, 0), 0)

  val partA = distance(solution._1).toString
  solution(partA, A)

  val partB = solution._2.toString
  solution(partB, B)
}
