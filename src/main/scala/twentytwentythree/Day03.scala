package twentytwentythree

import scala.io.Source

object Day03 extends App {
  val source = Source.fromResource("twentytwentythree/Day03.txt")
  val instructions = source.getLines.toList
  source.close

  val grid = instructions
    .zipWithIndex
    .flatMap {
      case (row, y) => row.split("").zipWithIndex.toList.map {
        case (char, x) => Point(x, y) -> char.head
      }
    }
    .toMap
  val ys = instructions.indices.toList
  val xs = instructions.head.indices.toList

  val groups = ys.flatMap { y =>
    xs
      .map(x => Point(x, y))
      .foldLeft(List.empty[List[Point]]) {
        case (Nil, curr) =>
          if (curr.isDigit) List(List(curr)) else Nil
        case (acc, curr) =>
          if (!curr.isDigit)
            acc
          // If the character on the left is a digit, we haven't
          // seen the entire number yet.
          else if (Point(curr.x - 1, curr.y).isDigit) {
            val init :+ last = acc
            init :+ last.appended(curr)
          } else acc :+ List(curr)
      }
      .map(PartNumber.apply)
  }

  case class Point(x: Int, y: Int) {
    def adjacent: List[Point] =
      List(
        (x, y - 1), // Top.
        (x + 1, y - 1), // Top right.
        (x + 1, y), // Right.
        (x + 1, y + 1), // Bottom right.
        (x, y + 1), // Bottom.
        (x - 1, y + 1), // Bottom left.
        (x - 1, y), // Left.
        (x - 1, y - 1), // Top left.
      ).map {
        case (x, y) => Point(x, y)
      }
    def char: Char = grid.getOrElse(this, '.')
    def isDigit: Boolean = char.isDigit
    def isSymbol: Boolean = !char.isDigit && char != '.'
  }
  case class PartNumber(points: List[Point]) {
    def partNumber: Int =
      points.sortBy(_.x).map(_.char).mkString.toInt
    def isAdjacentToSymbol: Boolean =
      points.exists(_.adjacent.exists(_.isSymbol))
    def isAdjacentToPoint(point: Point): Boolean =
      points.exists(_.adjacent.contains(point))
  }

  def solvePart1: Long = groups
    .filter(_.isAdjacentToSymbol)
    .map(_.partNumber)
    .sum

  def solvePart2: Long = {
    val asterisks = grid.keys.filter(_.char == '*')

    val asterisksAdjacentToTwoPartNumbers = asterisks
      .map {
        point => groups.filter(_.isAdjacentToPoint(point)).map(_.partNumber)
      }
      .filter(_.length == 2)

    asterisksAdjacentToTwoPartNumbers
      .map(_.product)
      .sum
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
