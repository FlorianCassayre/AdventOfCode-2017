package adventofcode.solutions

import adventofcode._

object Day07 extends Day(7) {

  val regex = "^([a-z]+) \\(([0-9]+)\\)(?: -> (.*)?)?$".r

  val values = input.split(lineSeparator).map{
    case regex(node, weight, nodes) => (node, weight.toInt, nodes match { case null => List() case c => c.split(", ").toList} )
  }

  val parents: Map[String, String] = values.flatMap{ case (parent, _, nodes) => nodes.map(child => child -> parent) }.toMap
  val children: Map[String, (Int, List[String])] = values.map{ case (node, weight, nodes) => node -> (weight, nodes) }.toMap

  def parent(node: String): String = if (parents.contains(node)) parent(parents(node)) else node

  val partA = parent(parents.head._1)
  solution(partA, A)

  def sum(node: String): Int = children(node) match {
    case (weight, Nil) => weight
    case (weight, list) => list.foldLeft(weight)((a, e) => a + sum(e))
  }

  def correctWeight(node: String): Int = {
    val (_, nodes) = children(node)
    val groups = nodes.groupBy(sum)
    if (groups.size == 1)
      children(node)._1 + children(parents(node))._2.groupBy(sum).toList.maxBy(_._2.size)._1 - sum(node)
    else
      correctWeight(groups.toList.minBy(_._2.size)._2.head)
  }

  val partB = correctWeight(partA).toString
  solution(partB, B)
}
