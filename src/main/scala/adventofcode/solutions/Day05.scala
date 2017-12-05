package adventofcode.solutions

import adventofcode._

object Day05 extends Day(5) {

  val lines = input.split(lineSeparator).map(_.toInt)

  def jump(count: Int, index: Int, array: Array[Int], f: Int => Int): Int = {
    if (index < 0 || index >= array.length)
      count
    else {
      val off = array(index)
      array(index) += f(off)
      jump(count + 1, off + index, array, f)
    }
  }

  val partA = jump(0, 0, lines.clone(), _ => 1).toString
  solution(partA, A)

  val partB = jump(0, 0, lines.clone(), i => if (i >= 3) -1 else 1).toString
  solution(partB, B)
}
