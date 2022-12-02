package twentytwentytwo

import scala.io.Source

object Day02 extends App {
  val source = Source.fromResource("twentytwentytwo/Day02.txt")
  val rawInstructions = source.getLines.toList.map(_.split(" "))
  source.close

  def getInstructions[A, B](fnA: String => A, fnB: String => B): List[(A, B)] =
    rawInstructions.map {
      case Array(a, b) => (fnA(a), fnB(b))
    }

  abstract case class Outcome(points: Int)
  object Outcome {
    def apply(str: String): Outcome =
      str match {
        case "X" => Lose
        case "Y" => Draw
        case "Z" => Win
      }
  }
  object Win extends Outcome(6)
  object Lose extends Outcome(0)
  object Draw extends Outcome(3)

  abstract case class Move(points: Int)
  object Move {
    def apply(letter: String): Move =
      letter match {
        case "A" | "X" => Rock
        case "B" | "Y" => Paper
        case "C" | "Z" => Scissors
      }
  }
  object Rock extends Move(1)
  object Paper extends Move(2)
  object Scissors extends Move(3)

  /** A map of a move to the move that it beats. */
  val beats: Map[Move, Move] = Map(
    Paper -> Rock,
    Scissors -> Paper,
    Rock -> Scissors,
  )

  def solvePart1: Long =
    getInstructions(Move.apply, Move.apply).foldLeft(0) { case (acc, (opponent, me)) =>
      val outcome =
        if (opponent == me) Draw
        else if (beats(me) == opponent) Win
        else Lose
      acc + me.points + outcome.points
    }

  def solvePart2: Long =
    getInstructions(Move.apply, Outcome.apply).foldLeft(0) { case (acc, (opponent, outcome)) =>
      val move = outcome match {
        case Win => beats.map(_.swap)(opponent)
        case Lose => beats(opponent)
        case Draw => opponent
      }
      acc + move.points + outcome.points
    }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
