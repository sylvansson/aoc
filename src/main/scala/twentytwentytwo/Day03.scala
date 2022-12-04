package twentytwentytwo

import scala.io.Source

object Day03 extends App {
  val source = Source.fromResource("twentytwentytwo/Day03.txt")
  val instructions = source.getLines.map(_.toCharArray).toList
  source.close

  val priorities = ((('a' to 'z') ++ ('A' to 'Z')) zip (1 to 52)).toMap

  def solvePart1: Long =
    instructions.foldLeft(0) { case (acc, curr) =>
      val (c1, c2) = curr.splitAt(curr.length / 2)
      acc + priorities((c1 intersect c2).head)
    }

  def solvePart2: Long =
    instructions.grouped(3).foldLeft(0) { case (acc, curr) =>
      acc + priorities(curr.reduceLeft(_ intersect _).head)
    }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
