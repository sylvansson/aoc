package twentytwentyone

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val source = Source.fromResource("twentytwentyone/Day10.txt")
  val instructions = source.getLines.toList.map(_.split("").toList)
  source.close

  val openers = List("(", "[", "{", "<")

  @tailrec
  def check(line: List[String], open: List[String]): (Int, List[String]) = {
    val scores = Map(
      ")" -> 3,
      "]" -> 57,
      "}" -> 1197,
      ">" -> 25137,
    )

    val closers = Map(
      "(" -> ")",
      "[" -> "]",
      "{" -> "}",
      "<" -> ">",
    )

    (line, open) match {
      case (Nil, open) =>
        (-1, open)
      case (char :: rest, open) if openers.contains(char) =>
        check(rest, open :+ char)
      case (char :: _, open) if char != closers(open.last) =>
        (scores(char), open)
      case (_ :: rest, open) =>
        check(rest, open.init)
    }
  }

  val (corruptedLines, incompleteLines) = instructions
    .map(line => check(line, Nil))
    .partition {
      case (score, _) => score != -1
    }

  def solvePart1: Long = corruptedLines.map(_._1).sum

  def solvePart2 = {
    val scores = incompleteLines
      .map(_._2)
      .map(open =>
        open.reverse.foldLeft(0L) {
          case (acc, curr) => (acc * 5) + (openers.indexOf(curr) + 1)
        }
      )
      .sorted

    scores(scores.size / 2)
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
