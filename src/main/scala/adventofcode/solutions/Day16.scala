package adventofcode.solutions

import adventofcode._

object Day16 extends Day(16) {

  val spin = "s([0-9]+)".r
  val exchange = "x([0-9]+)/([0-9]+)".r
  val partner = "p([a-p])/([a-p])".r

  sealed abstract class Move
  case class Spin(i: Int) extends Move
  case class Exchange(i: Int, j: Int) extends Move
  case class Partner(a: Char, b: Char) extends Move

  val initial = ('a' to 'p').toArray
  val instructions = input.split(",").map {
    case spin(i) => Spin(i.toInt)
    case exchange(i, j) => Exchange(i.toInt, j.toInt)
    case partner(a, b) => Partner(a.charAt(0), b.charAt(0))
  }.toList

  def move(array: Array[Char], list: List[Move]): Array[Char] = {
    list match {
      case Nil => array
      case head :: tail => move(head match {
        case Spin(i) =>
          val (x, y) = array.splitAt(array.length - i)
          y ++ x
        case Exchange(i, j) =>
          array.updated(i, array(j)).updated(j, array(i))
        case Partner(a, b) =>
          array.updated(array.indexOf(a), b).updated(array.indexOf(b), a)
      }, tail)
    }
  }

  val shuffled = move(initial, instructions)

  val partA = shuffled.mkString("")
  solution(partA, A)

  val times = 1000000000

  def reduceFast(current: Array[Char], times: Int): Int = {
    if (current.toList == initial.toList)
      times
    else
      reduceFast(move(current, instructions), times + 1)
  }

  def reduceSlow(current: Array[Char], times: Int): Array[Char] = {
    if (times == 0)
      current
    else
      reduceSlow(move(current, instructions), times - 1)
  }

  val partB = reduceSlow(initial, times % reduceFast(move(initial, instructions), 1)).mkString("")
  solution(partB, B)
}
