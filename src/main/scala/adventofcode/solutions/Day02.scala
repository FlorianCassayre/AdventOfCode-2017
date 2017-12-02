package adventofcode.solutions

import adventofcode._

object Day02 extends Day(2) {

  val array = input.split(lineSeparator).map(_.split("\t").map(_.toInt))

  val partA = array.map(a => a.max - a.min).sum.toString
  solution(partA, A)

  val partB = array.map(a => (for { i <- a; j <- a; if i != j && i % j == 0 } yield i / j).head).sum.toString
  solution(partB, B)
}
