package adventofcode.solutions

import adventofcode._

object Day20 extends Day(20) {

  val regexVector = "<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>".r
  val regexExpression = "p=(.+), v=(.+), a=(.+)".r

  case class Vector(x: Int, y: Int, z: Int) {
    def +(v: Vector): Vector = Vector(x + v.x, y + v.y, z + v.z)
    val manhattan: Int = this.productIterator.map{ case i: Int => Math.abs(i) }.sum
  }

  case class Particle(p: Vector, v: Vector, a: Vector) {
    def updated(): Particle = {
      val nv = v + a
      Particle(p + nv, nv, a)
    }
  }

  val particles = input.split(lineSeparator).map{ case regexExpression(p, v, a) =>
    val vectors = List(p, v, a).map{ case regexVector(pv, vv, va) => Vector(pv.toInt, vv.toInt, va.toInt) }
    Particle(vectors(0), vectors(1), vectors(2))
  }.toList


  val partA = particles.indexOf(particles.minBy(_.a.manhattan)).toString
  solution(partA, A)

  def collisions(n: Int, list: List[Particle]): List[Particle] = {
    if (n == 0)
      list
    else {
      val updated = list.map(_.updated())
      collisions(n - 1, updated.filter(p1 => updated.forall(p2 => p1 == p2 || p1.p != p2.p)))
    }
  }

  val partB = collisions(100, particles).size.toString
  solution(partB, B)
}
