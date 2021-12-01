package twentytwentyone

import scala.io.Source

object Day01 extends App {
  val source = Source.fromResource("twentytwentyone/Day01.txt")
  val instructions = source.getLines.map(_.toInt).toList
  source.close

  def solvePart1: Long = instructions
    .sliding(2)
    .count {
      case List(a, b) => b > a
    }

  def solvePart2: Long = instructions
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .toList
    .count {
      case Seq(a, b) => b > a
    }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
