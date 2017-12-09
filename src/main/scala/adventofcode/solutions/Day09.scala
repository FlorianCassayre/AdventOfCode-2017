package adventofcode.solutions

import adventofcode._

object Day09 extends Day(9) {

  val sanitized = input.replaceAll("!.", "")
  val regexClean = "<[^>]*>"
  val chars = sanitized.replaceAll(regexClean, "").replaceAll("[^\\{\\}]", "").toCharArray.toList

  def score(list: List[Char], acc: Int, level: Int): Int = list match {
    case Nil => acc
    case '{' :: tail => score(tail, acc + level, level + 1)
    case '}' :: tail => score(tail, acc, level - 1)
  }

  val partA = score(chars, 0, 1).toString
  solution(partA, A)

  def replacements(string: String, sum: Int): Int = {
    val replaced = string.replaceFirst(regexClean, "")
    if (string == replaced)
      sum
    else
      replacements(replaced, sum + string.length - replaced.length - 2)
  }

  val partB = replacements(sanitized, 0).toString
  solution(partB, B)
}
