package twentytwentytwo

import scala.io.Source
import utils._

object Day05 extends App {
  val source = Source.fromResource("twentytwentytwo/Day05.txt")
  val pattern = "move (?<n>\\d+) from (?<from>\\d+) to (?<to>\\d+)".r
  val instructions = source.getLines.toList
  source.close

  val stackInstructions :+ stackNumbers = instructions.takeWhile(_.trim.nonEmpty)
  val numStacks = stackNumbers.split(" ").last.toInt
  val moveInstructions = instructions.collect {
    case pattern(ExtractInt(n), ExtractInt(from), ExtractInt(to)) => Move(n, from - 1, to - 1)
  }

  type Stacks = List[List[Char]]

  case class Move(n: Int, from: Int, to: Int) {
    def apply(stacks: Stacks, reverse: Boolean): Stacks = {
      val (cratesToMove, fromRest) = stacks(from).splitAt(n)
      stacks
        .updated(to, (if (reverse) cratesToMove.reverse else cratesToMove) ++: stacks(to))
        .updated(from, fromRest)
    }
  }

  val cratePositions = (0 until numStacks).map(_ * 4 + 1)

  val inputStacks: Stacks = stackInstructions
    .map(line =>
      cratePositions.map(line.lift).map(_.filterNot(_ == ' '))
    )
    .foldLeft(Array.fill(numStacks)(List.empty[Char])) {
      case (acc, curr) =>
        acc.zip(curr).map {
          case (stack, maybeCrate) => stack ++ maybeCrate
        }
    }
    .toList

  def solve(reverse: Boolean): String = moveInstructions
    .foldLeft(inputStacks) {
      case (acc, curr) => curr(acc, reverse = reverse)
    }
    .map(_.head)
    .mkString

  println("Part 1: " + solve(reverse = true))
  println("Part 2: " + solve(reverse = false))
}
