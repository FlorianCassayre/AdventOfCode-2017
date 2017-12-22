package adventofcode.solutions

import adventofcode._

object Day22 extends Day(22) {

  case class Vector(x: Int, y: Int)

  val array = input.split(lineSeparator).map(_.toCharArray.map {
    case '.' => false
    case '#' => true
  })

  val initial: Set[Vector] = (for {
      i <- array.indices
      j <- array(i).indices
      if array(i)(j)
    } yield Vector(j - array(i).length / 2, i - array.length / 2))(collection.breakOut)

  def stepA(position: Vector, facing: Vector, set: Set[Vector], i: Int, count: Int): Int = {
    if (i == 0)
      count
    else {
      val (newFacing, newSet, newCount) = if (set.contains(position)) (Vector(-facing.y, facing.x), set - position, count) else (Vector(facing.y, -facing.x), set + position, count + 1)
      stepA(Vector(position.x + newFacing.x, position.y + newFacing.y), newFacing, newSet, i - 1, newCount)
    }
  }

  val partA = stepA(Vector(0, 0), Vector(0, -1), initial, 10000, 0).toString
  solution(partA, A)


  abstract class State
  case object Weakened extends State
  case object Infected extends State
  case object Flagged extends State

  def stepB(position: Vector, facing: Vector, map: Map[Vector, State], i: Int, count: Int): Int = {
    if (i == 0)
      count
    else {
      val (newFacing, newMap, newCount) = map.get(position) match {
        case None => (Vector(facing.y, -facing.x), map + (position -> Weakened), count)
        case Some(Weakened) => (facing, map + (position -> Infected), count + 1)
        case Some(Infected) => (Vector(-facing.y, facing.x), map + (position -> Flagged), count)
        case Some(Flagged) => (Vector(-facing.x, -facing.y), map - position, count)
      }
      stepB(Vector(position.x + newFacing.x, position.y + newFacing.y), newFacing, newMap, i - 1, newCount)
    }
  }

  val partB = stepB(Vector(0, 0), Vector(0, -1), initial.map(_ -> Infected).toMap, 10000000, 0).toString
  solution(partB, B)
}
