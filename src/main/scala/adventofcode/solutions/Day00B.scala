package adventofcode.solutions

import adventofcode.definition._

object Day00B extends Day(0, B) {

  /*
  Compute the sum of the squares of the inputs
   */

  lazy val output = input.split(" ").map(_.toInt).map(i => i * i).sum.toString

}
