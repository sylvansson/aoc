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

  /**
   * Slow approach to collapsing. We only destroy one pair of units
   * per recursive step.
   */
  @tailrec
  def slowCollapse(polymer: Array[Char]): Array[Char] = {
    val toCollapse = polymer
      .sliding(2)
      .zipWithIndex
      .collectFirst {
        case (Array(first, second), i) if areOppositePolarities(first, second) => i
      }
    toCollapse match {
      case None => polymer
      case Some(i) => slowCollapse(polymer.patch(i, Nil, 2))
    }
  }

  /**
   * More efficient approach to collapsing. We destroy as many pairs
   * as possible per recursive step.
   */
  @tailrec
  def collapse(polymer: Array[Char]): Array[Char] = {
    val toCollapse = polymer
      .sliding(2)
      .toList
      .zipWithIndex
      .filter {
        case (Array(a, b), _) => areOppositePolarities(a, b)
      }
      // This prevents us from destroying too many units when there are
      // more than 2 adjacent ones. For example, aAa.
      .collect {
        case (Array(_, b), i) if polymer.lift(i + 2).isEmpty || !areOppositePolarities(b, polymer(i + 2)) => i
      }

    if (toCollapse.isEmpty)
      return polymer

    val collapsedPolymer = toCollapse
      .zipWithIndex
      .foldLeft(polymer) {
        // We shift the index to account for the fact that other
        // units have been removed.
        case (acc, (positionInPolymer, i)) => acc.patch(positionInPolymer - (i * 2), Nil, 2)
      }

    collapse(collapsedPolymer)
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
