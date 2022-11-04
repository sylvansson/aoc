package twentyeighteen

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

object Day05 extends App {
  val source = Source.fromResource("twentyeighteen/Day05.txt")
  val polymer = source.getLines.toList.head.toCharArray
  source.close

  def areOppositePolarities(first: Char, second: Char) =
    first != second && first.toLower == second.toLower

  @tailrec
  def collapse(polymer: Array[Char]): Array[Char] = {
    val toCollapse = polymer
      .sliding(2)
      .zipWithIndex
      .collectFirst {
        case (Array(first, second), i) if areOppositePolarities(first, second) => i
      }
    toCollapse match {
      case None => polymer
      case Some(i) => collapse(polymer.patch(i, Nil, 2))
    }
  }

  def solvePart1: Int = collapse(polymer).length

  def solvePart2: Int = polymer
    .map(_.toLower)
    .distinct
    .par
    .map(u => collapse(polymer.filter(_.toLower != u)).length)
    .min

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
