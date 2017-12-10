package adventofcode.solutions

import adventofcode._

object Day10 extends Day(10) {

  val range = (0 until 256).toList

  def step(list: List[Int], lengths: List[Int], index: Int, skip: Int): (List[Int], Int, Int) = lengths match {
    case Nil => (list, index, skip)
    case length :: tail =>
      val reversed = reverse(list, index, length)
      step(reversed, tail, (index + length + skip) % list.size, skip + 1)
  }

  def reverse(list: List[Int], index: Int, length: Int): List[Int] = {
    val (first, tail1) = list.splitAt(Math.max(index + length - list.size, 0))
    val (mid, tail2) = tail1.splitAt(index - first.length)
    val (end, leftOver) = tail2.splitAt(Math.min(length, tail2.size))
    val (revOne, revTwo) = (end ++ first).reverse.splitAt(Math.min(index + length, list.size) - index)
    revTwo ++ mid ++ revOne ++ leftOver
  }

  val (hashedA, _, _) = step(range, input.split(",").map(_.toInt).toList, 0, 0)
  val partA = (hashedA.head * hashedA.tail.head).toString
  solution(partA, A)


  val appendix = List(17, 31, 73, 47, 23)
  val bytes = input.toCharArray.map(_.toInt).toList ++ appendix
  val (iterated, _, _) = (0 until 64).foldLeft((range, 0, 0)){ case ((list, index, skip), _) => step(list, bytes, index, skip) }
  val (xored, _) = (0 until 16).foldLeft((List[Byte](), iterated.reverse)){ case ((list, blocks), _) =>
    val (left, tail) = blocks.splitAt(16)
    (left.reduce(_ ^ _).toByte :: list, tail)
  }

  val partB = xored.map("%02X".format(_)).mkString("").toLowerCase
  solution(partB, B)
}
