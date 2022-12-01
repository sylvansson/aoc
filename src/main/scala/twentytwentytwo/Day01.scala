package twentytwentytwo

import scala.io.Source

object Day01 extends App {
  val source = Source.fromResource("twentytwentytwo/Day01.txt")
  val instructions = source.getLines.toList
  source.close

  val calories = instructions.foldLeft(List(0)) {
    case (acc, curr) if curr.trim == "" =>
      acc :+ 0
    case (acc, curr) =>
      acc.init :+ (acc.last + curr.toInt)
  }

  def solvePart1: Long = calories.max

  def solvePart2: Long = calories.sorted.takeRight(3).sum

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
