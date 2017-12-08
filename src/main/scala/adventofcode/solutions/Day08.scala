package adventofcode.solutions

import adventofcode._

object Day08 extends Day(8) {

  val regex = "([a-z]+) ([a-z]+) (-?[0-9]+) if ([a-z]+) (.+) (-?[0-9]+)".r

  val operations = Map[String, (Int, Int) => Boolean](
    ">" -> (_ > _),
    "<" -> (_ < _),
    ">=" -> (_ >= _),
    "<=" -> (_ <= _),
    "==" -> (_ == _),
    "!=" -> (_ != _)
  )

  val (map, max) = input.split(lineSeparator).foldLeft((Map[String, Int](), 0)){ case ((acc, m), regex(reg, sign, amountStr, test, op, valueStr)) =>
    val amount = amountStr.toInt
    if (operations(op)(acc.getOrElse(test, 0), valueStr.toInt)) {
      val updated = acc.getOrElse(reg, 0) + (sign match {
        case "inc" => amount
        case "dec" => -amount
      })
      (acc + (reg -> updated), Math.max(m, updated))
    }
    else
      (acc, m)
  }

  val partA = map.values.max.toString
  solution(partA, A)

  val partB = max.toString
  solution(partB, B)
}
