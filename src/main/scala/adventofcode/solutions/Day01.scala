package adventofcode.solutions

import adventofcode._

object Day01 extends Day(1) {

  val array = input.toCharArray.map(_.asDigit)

  val partA = array.indices.map(i => if (array(i) == array((i + 1) % array.length)) array(i) else 0).sum.toString
  solution(partA, A)

  val partB = array.indices.map(i => if (array(i) == array((i + array.length / 2) % array.length)) array(i) else 0).sum.toString
  solution(partB, B)
}
