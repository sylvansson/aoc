package twentyeighteen

import scala.io.Source

object Day02 extends App {
  val source = Source.fromResource("twentyeighteen/Day02.txt")
  val instructions = source.getLines.toList
  source.close

  def countChars(str: String) =
    str.groupBy(identity).view.mapValues(_.length)

  def solvePart1: Int = {
    val (twos, threes) = instructions.foldLeft((0, 0)) {
      case ((twos, threes), curr) =>
        val counts = countChars(curr)
        val newTwos = if (counts.values.exists(_ == 2)) twos + 1 else twos
        val newThrees = if (counts.values.exists(_ == 3)) threes + 1 else threes
        (newTwos, newThrees)
    }
    twos * threes
  }

  def solvePart2: String = instructions
    .combinations(2)
    .map {
      case List(a, b) => a.zip(b).partition(pair => pair._1 == pair._2)
    }
    .collectFirst {
      case (e, ne) if ne.length == 1 => e.map(_._1).mkString
    }
    .head

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
