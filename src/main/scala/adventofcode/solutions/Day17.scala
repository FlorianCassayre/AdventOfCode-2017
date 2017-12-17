package adventofcode.solutions

import adventofcode._

object Day17 extends Day(17) {

  val increment = input.toInt
  val range = 2017

  def step(pos: Int, count: Int, tape: List[Int]): List[Int] = {
    if (count > range)
      tape
    else {
      val newPos = 1 + ((pos + increment) % tape.size)
      val (left, right) = tape.splitAt(newPos)
      step(newPos, count + 1, left ++ List(count) ++ right)
    }
  }

  val list = step(0, 1, List(0))

  val partA = list(list.indexOf(range) + 1).toString
  solution(partA, A)

  val rangeBig = 50000000

  def iterated(afterZero: Int, pos: Int, count: Int): Int = {
    if (count > rangeBig)
      afterZero
    else {
      val newPos = 1 + ((pos + increment) % count)
      iterated(if (newPos == 1) count else afterZero, newPos, count + 1)
    }
  }

  val partB = iterated(0, 1, 1).toString
  solution(partB, B)
}
