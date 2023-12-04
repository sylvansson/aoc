package twentytwentythree

import scala.io.Source

object Day04 extends App {
  val source = Source.fromResource("twentytwentythree/Day04.txt")
  val instructions = source.getLines.toList
  source.close

  val cardPattern = "Card\\s+(?<cardId>\\d+):\\s+(?<winning>.+) \\|\\s+(?<hand>.+)".r

  def parseNumbers(strNumbers: String): Set[Int] =
    strNumbers.split("\\s+").map(_.toInt).toSet

  val numWinningByCardId: Map[Int, Int] = instructions
    .map {
      case cardPattern(cardId, winning, hand) =>
        val common = List(winning, hand).map(parseNumbers).reduceLeft(_ intersect _)
        cardId.toInt -> common.size
    }
    .toMap
  val numCards = numWinningByCardId.size

  def solvePart1: Long = numWinningByCardId
    .collect {
      case (_, winningCount) if winningCount > 0 =>
        Math.pow(2, winningCount - 1).toLong
    }
    .sum

  def solvePart2: Long = {
    val z = Map.empty[Int, Int].withDefaultValue(1)
    (1 to numCards)
      .foldLeft(z) {
        case (acc, currCardId) =>
          val cardsToCopy = (1 to numWinningByCardId(currCardId))
            .map(_ + currCardId)
            .filter(_ <= numCards)

          val updatedCounts = cardsToCopy.map(cardId =>
            cardId -> (acc(cardId) + acc(currCardId))
          )
          acc ++ updatedCounts
      }
      .values
      .sum
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
