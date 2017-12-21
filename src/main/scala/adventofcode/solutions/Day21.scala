package adventofcode.solutions

import adventofcode._

object Day21 extends Day(21) {

  type Matrix = Array[Array[Boolean]]

  val regex = "([\\.#/]+) => ([\\.#/]+)".r

  val startingPattern: Matrix = ".#.\n..#\n###".split(lineSeparator).map(_.toCharArray.map(charToBoolean))

  def charToBoolean(char: Char): Boolean = char match {
    case '.' => false
    case '#' => true
  }

  def patternParse(s: String): Matrix = s.split("/").map(_.toCharArray.map(charToBoolean))

  def rotate(array: Matrix, n: Int): Matrix = {
    if (n == 0)
      array
    else
      rotate((for (i <- array(0).indices) yield (for (j <- array.indices) yield array(j)(array(0).length - i - 1)).toArray).toArray, n - 1)
  }

  def subArray(array: Matrix, i: Int, j: Int, n: Int): Matrix = array.splitAt(i)._2.take(n).map(_.splitAt(j)._2.take(n))

  val patterns = input.split(lineSeparator).map{ case regex(pattern, replacement) => (patternParse(pattern), patternParse(replacement)) }

  val dictonary = patterns.flatMap{ case (p, r) => (0 until 4).flatMap(n => Set(rotate(p, n).deep -> r, rotate(p, n).map(_.reverse).deep -> r)) }.toMap

  def iterate(n: Int, array: Matrix): Matrix = {
    if (n == 0)
      array
    else {
      val size = if (array.length % 2 == 0) 2 else 3

      val arrays = for {
        i <- 0 until array.length / size
        line = for (j <- 0 until array.length / size) yield subArray(array, i * size, j * size, size)
      } yield line

      val mapped = arrays.map(_.map(sub => dictonary(sub.deep)))

      val reduced: Matrix = (for {
        i <- mapped.indices
        line = mapped(i)
      } yield line.reduce((a1, a2) => for (k <- a1.indices.toArray) yield a1(k) ++ a2(k))).flatten.toArray

      iterate(n - 1, reduced)
    }
  }


  val partA = iterate(5, startingPattern).flatten.count(identity).toString
  solution(partA, A)

  val partB = iterate(18, startingPattern).flatten.count(identity).toString
  solution(partB, B)
}
