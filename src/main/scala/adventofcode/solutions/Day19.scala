package adventofcode.solutions

import adventofcode._

object Day19 extends Day(19) {

  val array = input.split(lineSeparator).map(_.toCharArray)

  case class Vector(x: Int, y: Int)

  val directions = Set(Vector(1, 0), Vector(0, 1), Vector(-1, 0), Vector(0, -1))

  def isInside(point: Vector): Boolean = point.x >= 0 && point.x < array(0).length && point.y >= 0 && point.y < array.length

  def add(v1: Vector, v2: Vector): Vector = Vector(v1.x + v2.x, v1.y + v2.y)

  def move(position: Vector, direction: Vector, letters: List[Char], steps: Int): (List[Char], Int) = {
    array(position.y)(position.x) match {
      case '|' | '-' => move(add(position, direction), direction, letters, steps + 1)
      case '+' =>
        val newDirection = (directions - Vector(-direction.x, -direction.y)).filter(v => isInside(add(position, v)))
          .find(v => array(position.y + v.y)(position.x + v.x) != ' ').get
        move(add(position, newDirection), newDirection, letters, steps + 1)
      case ' ' => (letters.reverse, steps)
      case letter => move(add(position, direction), direction, letter :: letters, steps + 1)
    }
  }

  val (sequence, steps) = move(Vector(array(0).indexOf('|'), 0), Vector(0, 1), Nil, 0)

  val partA = sequence.mkString
  solution(partA, A)

  val partB = steps.toString
  solution(partB, B)
}
