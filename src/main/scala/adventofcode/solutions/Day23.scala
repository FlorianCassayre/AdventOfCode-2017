package adventofcode.solutions

import adventofcode._

object Day23 extends Day(23) {

  type Registers = Map[Char, Long]

  val (pc, count) = ('p', 'm')

  val regex = "([a-z]{3}) ([a-h]|-?[0-9]+) ([a-h]|-?[0-9]+)".r

  val actions: Array[Registers => Registers] = input.split(lineSeparator).map {
    case regex(mnem, register, value) =>
      map: Registers => {
        val r = register.charAt(0)
        val rl = if (register.charAt(0).isLetter) map(register.charAt(0)) else register.toInt
        val v = if (value.charAt(0).isLetter) map(value.charAt(0)) else value.toInt
        val updated = mnem match {
          case "set" => map + (r -> v)
          case "sub" => map + (r -> (rl - v))
          case "mul" => map + (r -> (rl * v)) + (count -> (map(count) + 1))
          case "jnz" =>
            map + (pc -> (if (rl != 0) map(pc) + v - 1 else map(pc)))
        }
        updated + (pc -> (updated(pc) + 1))
      }
  }

  def run(registers: Registers): Registers = {
    val i = registers(pc)
    if (i < 0 || i >= actions.length)
      registers
    else
      run(actions(i.toInt)(registers))
  }

  val initial = ('a' to 'h').map(_ -> 0L).toMap + (pc -> 0L) + (count -> 0L)

  val partA = run(initial)(count).toString
  solution(partA, A)

  val initialB = ('a' to 'h').map(_ -> 0L).toMap + (pc -> 0L) + (count -> 0L) + ('a' -> 1L)

  val partB = run(initialB)(count).toString
  solution(partB, B)
}
