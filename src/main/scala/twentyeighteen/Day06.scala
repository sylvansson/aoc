package twentyeighteen

import scala.io.Source
import scala.util.Random

object Day06 extends App {
  val source = Source.fromResource("twentyeighteen/Day06.txt")
  val inputCoordinates = source
    .getLines
    .map(_.split(", "))
    .map {
      case Array(x, y) => Coordinates(x.toInt, y.toInt)
    }
    .toList
  source.close

  val maxX = inputCoordinates.maxBy(_.x).x
  val maxY = inputCoordinates.maxBy(_.y).y

  /**
   * All coordinates in the grid, not just the ones in the input.
   */
  val allCoordinates = for {
    x <- 0 to maxX
    y <- 0 to maxY
  } yield Coordinates(x, y)

  case class Coordinates(x: Int, y: Int) {
    val id: String = Random.alphanumeric.take(5).mkString

    def distance(other: Coordinates): Int =
      Math.abs(this.x - other.x) + Math.abs(this.y - other.y)

    def isAtBoundary: Boolean =
      x == 0 || y == 0 || x == maxX || y == maxY

    def isNotAtBoundary: Boolean = !isAtBoundary
  }

  def isFinite(group: List[Coordinates]): Boolean =
    group.forall(_.isNotAtBoundary)

  def solvePart1: Int = {
    val groups = allCoordinates.foldLeft(Map.empty[String, List[Coordinates]].withDefaultValue(Nil)) {
      case (acc, curr) =>
        val coordsByDistance = inputCoordinates.groupBy(_.distance(curr))

        coordsByDistance(coordsByDistance.keys.min) match {
          case closest :: rest if rest.isEmpty =>
            acc + (closest.id -> (acc(closest.id) :+ curr))
          // There's a tie.
          case _ => acc
        }
    }

    groups.values.filter(isFinite).maxBy(_.length).length
  }

  def solvePart2: Int =
    allCoordinates.count(coords =>
      inputCoordinates.map(_.distance(coords)).sum < 10000
    )

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
