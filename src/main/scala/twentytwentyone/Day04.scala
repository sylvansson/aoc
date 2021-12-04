package twentytwentyone

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source

object Day04 extends App {
  val source = Source.fromResource("twentytwentyone/Day04.txt")
  val strNums :: instructions = source.getLines.toList
  val nums = strNums.split(",").map(_.toInt).toList
  source.close

  def getBoards(instructions: List[String]): List[Board] = {
    val rows = instructions
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("\\s+").map(_.toInt).toList)

    rows.grouped(5).map(Board.apply).toList
  }

  case class Board(rows: List[List[Int]]) {
    val columns: List[List[Int]] = rows.transpose

    def hasWon(seen: List[Int]): Boolean =
      (rows ++ columns).exists(_.forall(seen.contains))

    def unmarked(seen: List[Int]): List[Int] =
      rows.flatten.filterNot(seen.contains)
  }

  @tailrec
  def solve(i: Int = 0, wins: ListMap[Int, Int] = ListMap.empty): ListMap[Int, Int] = {
    if (i == nums.length) {
      return wins
    }

    val seen = nums.take(i)
    val newWins = boards.zipWithIndex.collect {
      case (b, bIndex) if b.hasWon(seen) && !wins.contains(bIndex) =>
        (bIndex, b.unmarked(seen).sum * seen.last)
    }

    solve(i + 1, wins ++ newWins)
  }

  val boards = getBoards(instructions)

  println("Part 1: " + solve().head._2)
  println("Part 2: " + solve().last._2)
}
