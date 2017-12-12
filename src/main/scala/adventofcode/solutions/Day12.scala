package adventofcode.solutions

import adventofcode._

object Day12 extends Day(12) {

  val edges = input.split(lineSeparator).map(line => {
    val array = line.split(" <-> ")
    (array(0).toInt, array(1).split(", ").map(_.toInt))
  }).flatMap{ case (a, bs) => bs.flatMap(e => List((a, e), (e, a))) }.toSet

  def tree(visited: Set[Int]): Set[Int] = {
    val next = visited.flatMap(e => edges.filter(_._1 == e).map(_._2)).diff(visited)
    if (next.isEmpty)
      visited
    else
      tree(visited ++ next)
  }

  val partA = tree(Set(0)).size.toString
  solution(partA, A)

  def groups(visited: Set[Int], count: Int): Int =  {
    val next = edges.map(_._1).diff(visited)
    if (next.isEmpty)
      count
    else
      groups(visited ++ tree(Set(next.head)), count + 1)
  }

  val partB = groups(Set(0), 0).toString
  solution(partB, B)
}
