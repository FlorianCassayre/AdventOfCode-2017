package adventofcode.solutions

import adventofcode._

object Day25 extends Day(25) {

  val generalRegex = """Begin in state ([A-Z])\.
                       |Perform a diagnostic checksum after ([0-9]+) steps\.
                       |
                       |([\s\S]+)""".stripMargin.r

  val stateRegex = """In state ([A-Z])\:
                     |  If the current value is 0\:
                     |    \- Write the value ([01])\.
                     |    \- Move one slot to the (left|right)\.
                     |    \- Continue with state ([A-Z])\.
                     |  If the current value is 1\:
                     |    \- Write the value ([01])\.
                     |    \- Move one slot to the (left|right)\.
                     |    \- Continue with state ([A-Z])\.""".stripMargin.r

  val (initialState, steps, states) = input match { case generalRegex(i, s, t) => (i.charAt(0), s.toInt, t) }

  def direction(string: String): Int = string match { case "left" => -1 case "right" => 1 }

  def write(i: Int, set: Set[Int], string: String): Set[Int] = string match { case "0" => set - i case "1" => set + i }

  val instructions = states.split("\n\n").map{ case stateRegex(state, writeA, moveA, continueA, writeB, moveB, continueB) =>
    state.charAt(0) -> ((current: Int, set: Set[Int]) =>
      if (!set.contains(current))
        (current + direction(moveA), write(current, set, writeA), continueA.charAt(0))
      else
        (current + direction(moveB), write(current, set, writeB), continueB.charAt(0))
      )
  }.toMap

  def run(current: Int, state: Char, set: Set[Int], i: Int): Set[Int] = {
    if (i == 0)
      set
    else {
      val (nextPointer, nextSet, nextState) = instructions(state)(current, set)
      run(nextPointer, nextState, nextSet, i - 1)
    }
  }

  val partA = run(0, initialState, Set(), steps).size.toString
  solution(partA, A)

  val partB = ???
  solution(partB, B)
}
