package adventofcode.solutions

import adventofcode._

object Day14 extends Day(14) {

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

  def knotHash(list: List[Int]): List[Byte] = {
    val appendix = List(17, 31, 73, 47, 23)
    val bytes = list ++ appendix
    val (iterated, _, _) = (0 until 64).foldLeft((range, 0, 0)){ case ((iterable, index, skip), _) => step(iterable, bytes, index, skip) }
    val (xored, _) = (0 until 16).foldLeft((List[Byte](), iterated.reverse)){ case ((iterable, blocks), _) =>
      val (left, tail) = blocks.splitAt(16)
      (left.reduce(_ ^ _).toByte :: iterable, tail)
    }
    xored
  }

  val size = 128
  val rows = (0 until size).map(i => (input + "-" + i).toCharArray.map(_.toInt).toList).map(knotHash)

  val partA = rows.map(_.map(i => Integer.bitCount(i & 0xff)).sum).sum.toString
  solution(partA, A)

  def byteToBits(byte: Int, acc: List[Boolean]): List[Boolean] = {
    if (acc.size == 8)
      acc
    else
      byteToBits(byte >>> 1, ((byte & 1) == 1) :: acc)
  }

  type Point = (Int, Int)

  def inBounds(p: Point): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < size && p._2 < size

  val array = rows.map(_.flatMap(byteToBits(_, Nil)).toArray).toArray

  val regions = (for(x <- 0 until size; y <- 0 until size; if array(x)(y)) yield (x, y)).toSet

  val sides = Set((0, 1), (1, 0), (-1, 0), (0, -1))

  def tree(visited: Set[Point]): Set[Point] = {
    val next = for {
      cell <- visited
      side <- sides
      position = (cell._1 + side._1, cell._2 + side._2)
      if inBounds(position) && array(position._1)(position._2)
      if !visited.contains(position)
    } yield position
    if (next.isEmpty)
      visited
    else
      tree(visited ++ next)
  }

  def groups(visited: Set[Point], count: Int): Int =  {
    val next = regions.diff(visited)
    if (next.isEmpty)
      count
    else
      groups(visited ++ tree(Set(next.head)), count + 1)
  }

  val partB = groups(Set(), 0).toString
  solution(partB, B)
}
