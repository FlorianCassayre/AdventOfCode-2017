package adventofcode.solutions

import adventofcode._

object Day06 extends Day(6) {

  val seq = input.split("\t").map(_.toInt).toSeq

  def allocate(count: Int, seq: Seq[Int], set: Map[Seq[Int], Int]): (Int, Int) = {
    if (set.contains(seq))
      (count, count - set(seq))
    else {
      val max = seq.max
      val index = seq.indexOf(max)
      allocate(count + 1, insert((index + 1) % seq.size, max, seq.updated(index, 0)), set + (seq -> count))
    }
  }

  def insert(index: Int, blocks: Int, seq: Seq[Int]): Seq[Int] = {
    if (blocks == 0)
      seq
    else
      insert((index + 1) % seq.size, blocks - 1, seq.updated(index, seq(index) + 1))
  }

  val (count, relative) = allocate(0, seq, Map())

  val partA = count.toString
  solution(partA, A)

  val partB = relative.toString
  solution(partB, B)
}
