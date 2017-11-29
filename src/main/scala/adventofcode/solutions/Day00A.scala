package adventofcode.solutions

import adventofcode.definition._

object Day00A extends Day(0, A) {

  /*
  Compute the sum of the inputs
   */

  lazy val output = input.split(" ").map(_.toInt).sum.toString

}
