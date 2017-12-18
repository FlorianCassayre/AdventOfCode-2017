package adventofcode.solutions

import adventofcode._

object Day18 extends Day(18) {

  val pc = ':'
  val sound = '-'

  val oneArgument = "([a-z]+) ([a-z])".r
  val twoArguments = "([a-z]+) ([a-z]) (.+)".r
  val anyArgument = "([a-z]+) (.+) (.+)".r

  def step(registers: Map[Char, Long], instructions: Array[String]): Map[Char, Long] = {
    val i = registers(pc)
    if (i < 0 || i >= instructions.length)
      registers
    else {
      val updated: Map[Char, Long] = instructions(i.toInt) match {
        case oneArgument(mnem, arg1) =>
          val a1: Long = registers.getOrElse(arg1.charAt(0), 0)
          mnem match {
            case "snd" => registers + (sound -> a1)
            case "rcv" => if (a1 != 0) registers + (pc -> -2) else registers
          }
        case twoArguments(mnem, arg1, arg2) =>
          val a1c = arg1.charAt(0)
          val a1: Long = registers.getOrElse(a1c, 0)
          val a2: Long = if (arg2.charAt(0).isLetter) registers.getOrElse(arg2.charAt(0), 0) else arg2.toLong
          mnem match {
            case "set" => registers + (a1c -> a2)
            case "add" => registers + (a1c -> (a1 + a2))
            case "mul" => registers + (a1c -> (a1 * a2))
            case "mod" => registers + (a1c -> (a1 % a2))
            case "jgz" => registers + (pc -> (if (a1 > 0) i + a2 - 1 else i))
          }
        case anyArgument(mnem, arg1, arg2) => mnem match {
          case "jgz" => registers + (pc -> (if (arg1.toLong > 0) i + arg2.toLong - 1 else i))
        }
      }
      step(updated + (pc -> (updated(pc) + 1)), instructions)
    }
  }

  val endState = step(Map(pc -> 0, sound -> 0), input.split(lineSeparator))

  val partA = endState(sound).toString
  solution(partA, A)


  // See commit message.
  val partB = ???
  solution(partB, B)
}
