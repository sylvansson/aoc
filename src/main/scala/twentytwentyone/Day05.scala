package twentytwentyone

import scala.io.Source

object Day05 extends App {
  val source = Source.fromResource("twentytwentyone/Day05.txt")
  val instructions = source.getLines.toList
  source.close

  val pattern = "(?<x1>\\d+),(?<y1>\\d+) -> (?<x2>\\d+),(?<y2>\\d+)".r

  def expand(a: (Int, Int), b: (Int, Int), includeDiagonals: Boolean): Seq[(Int, Int)] = {
    val (x1, y1) = a
    val (x2, y2) = b

    def coordRange(coord1: Int, coord2: Int): Range =
      coord1 to coord2 by (if (coord2 < coord1) -1 else 1)

    if (x1 == x2 || y1 == y2)
      for {
        x <- coordRange(x1, x2)
        y <- coordRange(y1, y2)
      } yield (x, y)
    else if (!includeDiagonals)
      Seq.empty
    else
      coordRange(x1, x2) zip coordRange(y1, y2)
  }

  def solve(includeDiagonals: Boolean): Long = {
    val points = instructions.flatMap {
      case pattern(x1, y1, x2, y2) =>
        expand((x1.toInt, y1.toInt), (x2.toInt, y2.toInt), includeDiagonals)
    }
    points.groupBy(identity).count(_._2.size >= 2)
  }

  println("Part 1: " + solve(false))
  println("Part 2: " + solve(true))
}
