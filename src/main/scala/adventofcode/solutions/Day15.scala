package adventofcode.solutions

import adventofcode._

object Day15 extends Day(15) {

  val regex = "Generator A starts with ([0-9]+)\nGenerator B starts with ([0-9]+)".r

  val (initialA, initialB) = input match { case regex(a, b) => (a.toLong, b.toLong) }
  val (factorA, factorB) = (16807L, 48271L)
  val mod = 2147483647
  val mask = 0xffff

  def genNext(n: Long, factor: Long, condition: Int): Long = {
    val next = (n * factor) % mod
    if ((next & condition) == 0)
      next
    else
      genNext(next, factor, condition)
  }

  def iterate(times: Int, maskA: Int, maskB: Int): Int = {
    val (_, _, count) = (0 until times).foldLeft((initialA, initialB, 0)){ case ((a, b, c), _) =>
      val (nextA, nextB) = (genNext(a, factorA, maskA), genNext(b, factorB, maskB))
      (nextA, nextB, c + (if ((nextA & mask) == (nextB & mask)) 1 else 0))
    }
    count
  }

  val partA = iterate(40000000, 0, 0).toString
  solution(partA, A)

  val partB = iterate(5000000, 3, 7).toString
  solution(partB, B)
}
