package adventofcode

import java.io.PrintWriter

import scala.io.Source

abstract class Day(day: Int) extends App {

  require(day >= 1 && day <= 25)

  protected val lineSeparator: String = "\n"

  private val formatted = "%02d".format(day)

  private def readInput(): String = Source.fromFile(s"input/$formatted.txt").getLines().mkString(lineSeparator)

  private def writeOutput(output: String, subProblem: SubProblem): Unit = {
    val sub = subProblem match {
      case A => "A"
      case B => "B"
    }
    new PrintWriter(s"output/$formatted$sub.txt") {
      write(output)
      close()
    }
  }

  protected val input: String = readInput()

  protected def solution(output: String, subProblem: SubProblem): Unit = {
    writeOutput(output, subProblem)
    println(output)
  }

}

sealed trait SubProblem
case object A extends SubProblem
case object B extends SubProblem