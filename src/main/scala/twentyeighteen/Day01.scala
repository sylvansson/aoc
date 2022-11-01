package twentyeighteen

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {
  val source = Source.fromResource("twentyeighteen/Day01.txt")
  val instructions = source.getLines.map(_.toInt).toList
  source.close

  def solvePart1: Int = instructions.sum

  def solvePart2: Int = {
    @tailrec
    def loop(
      acc: Set[Int] = Set.empty,
      curr: Int = 0,
      remaining: List[Int] = instructions,
    ): Int = {
      val x :: rest = if (remaining.nonEmpty) remaining else instructions
      val nextCurr = curr + x

      if (acc.contains(nextCurr))
        nextCurr
      else
        loop(acc + nextCurr, nextCurr, rest)
    }

    loop()
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
