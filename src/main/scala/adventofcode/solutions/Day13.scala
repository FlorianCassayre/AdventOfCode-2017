package adventofcode.solutions

import adventofcode._

object Day13 extends Day(13) {

  val map = input.split(lineSeparator).map(line => {
    val split = line.split(": ")
    split(0).toInt -> split(1).toInt
  }).toMap

  val range = map.keys.max

  def severity(i: Int, positions: Map[Int, Int], sum: Int): Int = {
    if (i > range + 1)
      sum
    else {
      val added = sum + (if (positions.contains(i) && positions(i) == 0) i * map(i) else 0)
      severity(i + 1, move(positions), added)
    }
  }

  def move(positions: Map[Int, Int]): Map[Int, Int] = positions.map{ case (k, v) => k -> (if (v == map(k) - 1) 2 - map(k) else v + 1) }

  val partA = severity(0, map.mapValues(_ => 0), 0).toString
  solution(partA, A)

  def findDelay(delay: Int, positions: Map[Int, Int]): Int = {
    if (isSafe(0, positions))
      delay
    else
      findDelay(delay + 1, move(positions))
  }

  def isSafe(i: Int, positions: Map[Int, Int]): Boolean = {
    if (i > range + 1)
      true
    else {
      if (positions.contains(i) && positions(i) == 0)
        false
      else
        isSafe(i + 1, move(positions))
    }
  }

  val partB = findDelay(0, map.mapValues(_ => 0)).toString
  solution(partB, B)
}
