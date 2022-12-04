package twentytwentytwo

import scala.io.Source

object Day04 extends App {
  val source = Source.fromResource("twentytwentytwo/Day04.txt")
  val pattern = "(?<a1>\\d+)-(?<a2>\\d+),(?<b1>\\d+)-(?<b2>\\d+)".r
  val instructions = source.getLines.toList.map {
    case pattern(a1, a2, b1, b2) => (a1.toInt to a2.toInt, b1.toInt to b2.toInt)
  }
  source.close

  def solvePart1: Long = {
    instructions.count {
      case (a, b) =>
        val List(shortest, longest) = List(a, b).sortBy(_.length)
        longest.contains(shortest.start) && longest.contains(shortest.end)
    }
  }

  def solvePart2: Long =
    instructions.count {
      case (a, b) => a.contains(b.start) || b.contains(a.start)
    }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
