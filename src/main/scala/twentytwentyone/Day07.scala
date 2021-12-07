package twentytwentyone

import scala.io.Source

object Day07 extends App {
  val source = Source.fromResource("twentytwentyone/Day07.txt")
  val instructions = source.getLines.toList.head.split(",").map(_.toInt).toList
  source.close

  def solvePart1: Long = positions
    .map(pos => instructions.map(x => (x - pos).abs).sum)
    .min

  def solvePart2: Long = positions
    .map(pos => instructions.map(x => (0 to (x - pos).abs).sum).sum)
    .min

  val minPos = instructions.min
  val maxPos = instructions.max
  val positions = minPos to maxPos

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
