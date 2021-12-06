package twentytwentyone

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App {
  val source = Source.fromResource("twentytwentyone/Day06.txt")
  val instructions = source.getLines.toList.head.split(",").map(_.toInt).toList
  source.close

  def solve(numDays: Int): Long = {
    @tailrec
    def loop(day: Int, fish: Map[Int, Long]): Long = {
      if (day == numDays) {
        return fish.values.sum
      }

      val updatedFish = fish.toList
        .map {
          case (timer, count) => (if (timer == 0) 6 else timer - 1, count)
        }
        .groupBy(_._1)
        .view
        // Merge the counts for fish that have been reset,
        // and fish that went from 7 to 6.
        .mapValues(_.map(_._2).sum)
        .toMap
        .updated(8, fish.getOrElse(0, 0.toLong))

      loop(day + 1, updatedFish)
    }

    val fish = instructions.groupBy(identity).view.mapValues(_.size.toLong).toMap
    loop(0, fish)
  }

  println("Part 1: " + solve(80))
  println("Part 2: " + solve(256))
}
