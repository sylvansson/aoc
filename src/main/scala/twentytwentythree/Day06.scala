package twentytwentythree

import scala.io.Source

object Day06 extends App {
  val source = Source.fromResource("twentytwentythree/Day06.txt")
  val instructions = source.getLines.toList
  source.close

  case class Race(time: Long, distance: Long) {
    def numOptions: Long =
      (1L until this.time).count {
        holdTime =>
          val moveTime = this.time - holdTime
          val distanceTravelled = moveTime * holdTime
          distanceTravelled > this.distance
      }
  }

  def races(join: Boolean): Array[Race] = {
    val List(times, distances) = instructions.map { insts =>
      val values = insts.split(":")(1).trim.split("\\s+")
      if (join)
        Array(values.mkString.toLong)
      else values.map(_.toLong)
    }

    times.zip(distances).map {
      case (time, distance) => Race(time, distance)
    }
  }

  def solvePart1: Long =
    races(join = false).map(_.numOptions).product

  def solvePart2: Long =
    races(join = true).map(_.numOptions).product

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
