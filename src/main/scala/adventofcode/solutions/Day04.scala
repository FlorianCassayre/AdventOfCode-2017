package adventofcode.solutions

import adventofcode._

object Day04 extends Day(4) {

  val phrases = input.split(lineSeparator).map(_.split(" "))

  val partA = phrases.count(_.groupBy(identity).values.forall(_.length == 1)).toString
  solution(partA, A)

  val partB = phrases.count(_.map(_.toCharArray.groupBy(identity).mapValues(_.length)).groupBy(identity).values.forall(_.length == 1)).toString
  solution(partB, B)
}
