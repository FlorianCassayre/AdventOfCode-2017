package adventofcode.definition

import java.io.PrintWriter

import scala.io.Source

sealed trait SubProblem
case object A extends SubProblem
case object B extends SubProblem

abstract class Day(day: Int, subProblem: SubProblem) extends App {

  require(day >= 0 && day <= 25)

  protected val lineSeparator: String = "\n"

  private val formatted = "%02d".format(day)

  private def readInput(): String = Source.fromFile(s"input/$formatted.txt").getLines().mkString(lineSeparator)

  private def writeOutput(output: String): Unit = {
    val sub = subProblem match {
      case A => "A"
      case B => "B"
    }
    new PrintWriter(s"output/$formatted$sub.txt") {
      write(output)
      close()
    }
    println(output)
  }

  protected val input: String = readInput()

  protected val output: String

  assert(output != null, "overriden output variable must be lazy")
  writeOutput(output)

}
