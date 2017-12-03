package adventofcode.solutions

import adventofcode._

object Day03 extends Day(3) {

  type Vector = (Int, Int) // (x, y)
  type SpiralCursor = (Vector, Vector, Set[Vector]) // (position, direction, visited)

  def add(v1: Vector, v2: Vector): Vector = (v1._1 + v2._1, v1._2 + v2._2)

  def step(cursor: SpiralCursor): SpiralCursor = {
    val (position, direction, set) = cursor
    val turn = direction match {
      case (1, 0) => (0, 1)
      case (0, 1) => (-1, 0)
      case (-1, 0) => (0, -1)
      case (0, -1) => (1, 0)
    }

    val nextTurn = add(position, turn)
    val nextStraight = add(position, direction)

    if (set.contains(nextTurn))
      (nextStraight, direction, set + nextStraight)
    else
      (nextTurn, turn, set + nextTurn)
  }

  def distance(p: Vector): Int = Math.abs(p._1) + Math.abs(p._2)

  val n = input.toInt

  val initial = ((0, 0), (0, -1), Set((0, 0)))

  val streamA: Stream[SpiralCursor] = initial #:: streamA.map {e => step(e)}
  val (positionA, _, _) = streamA(n - 1)
  val partA = distance(positionA).toString
  solution(partA, A)


  val streamB: Stream[(SpiralCursor, Map[Vector, Int])] = (initial, Map((0, 0) -> 1)) #:: streamB.map { e =>
    val (cursor, map) = e
    val next = step(cursor)
    val sum = (for(i <- -1 to 1; j <- -1 to 1; if i != 0 || j != 0) yield map.getOrElse(add(next._1, (i, j)), 0)).sum
    (next, map + (next._1 -> sum))
  }
  val partB = streamB.find{ case ((position, _, _), map) => map.getOrElse(position, 0) >= n } match {
    case Some(e) => e._2(e._1._1).toString
  }
  solution(partB, B)
}
