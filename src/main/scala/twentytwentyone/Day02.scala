package twentytwentyone

import scala.io.Source

object Day02 extends App {
  val source = Source.fromResource("twentytwentyone/Day02.txt")
  val instructions = source.getLines.toList
    .map(_.split(" "))
    .map {
      case Array(dir, offset) => Instruction(dir, offset.toInt)
    }
  source.close

  case class Instruction(dir: String, offset: Int)

  def solvePart1: Long = {
    val (x, depth) = instructions.foldLeft((0, 0)) {
      case ((x, depth), curr) =>
        curr.dir match {
          case "forward" => (x + curr.offset, depth)
          case "up" => (x, depth - curr.offset)
          case "down" => (x, depth + curr.offset)
        }
    }
    x * depth
  }

  def solvePart2: Long = {
    val (x, depth, _) = instructions.foldLeft((0, 0, 0)) {
      case ((x, depth, aim), curr) =>
        curr.dir match {
          case "forward" => (x + curr.offset, depth + (aim * curr.offset), aim)
          case "up" => (x, depth, aim - curr.offset)
          case "down" => (x, depth, aim + curr.offset)
        }
    }
    x * depth
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
