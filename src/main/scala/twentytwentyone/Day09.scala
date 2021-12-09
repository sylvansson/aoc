package twentytwentyone

import scala.io.Source

object Day09 extends App {
  val source = Source.fromResource("twentytwentyone/Day09.txt")
  val instructions = source.getLines.toList.map(_.split("").map(_.toInt))
  source.close

  case class Point(x: Int, y: Int) {
    def isValid: Boolean = xBounds.contains(x) && yBounds.contains(y)
    def resolve: Int = instructions(y)(x)

    def adjacent: List[Point] =
      List(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1))
        .filter(_.isValid)
  }

  val xBounds = instructions.head.indices
  val yBounds = instructions.indices
  val points = for (x <- xBounds; y <- yBounds) yield Point(x, y)

  def solvePart1: Long = points
    .filter(point => point.adjacent.forall(_.resolve > point.resolve))
    .map(_.resolve + 1)
    .sum

  def solvePart2: Int = {
    // We duplicate the list of points to compute basins properly.
    // Not ideal but it works.
    val basins = (points ++ points.reverse ++ points ++ points.reverse).foldLeft(List.empty[Set[Point]]) {
      case (basins, point) =>
        if (point.resolve == 9) {
          basins
        } else {
          // Look for an existing basin that's adjacent to the
          // current point.
          val maybeIndex = basins.indexWhere {
            basin => basin.exists(p => point.adjacent.contains(p))
          }
          maybeIndex match {
            case -1 => basins :+ Set(point)
            case i => basins.updated(i, basins(i) + point)
          }
        }
    }

    basins.sortBy(_.size)(Ordering.Int.reverse).take(3).map(_.size).product
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
