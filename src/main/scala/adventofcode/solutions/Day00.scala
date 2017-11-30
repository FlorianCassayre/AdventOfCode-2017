package adventofcode.solutions

import adventofcode._

object Day00 extends Day(0) {

  /*
  Compute the sum of the inputs
   */

  val partA = input.split(" ").map(_.toInt).sum.toString
  solution(partA, A)

  /*
  Compute the sum of the squares of the inputs
  */

  val partB = input.split(" ").map(_.toInt).map(i => i * i).sum.toString
  solution(partB, B)

}
