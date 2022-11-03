package twentyeighteen

import scala.io.Source

object Day04 extends App {
  val source = Source.fromResource("twentyeighteen/Day04.txt")
  val recordPattern = ".+:(?<minute>\\d{2})] (?<text>.+)".r
  val guardPattern = "Guard #(?<guardId>\\d+) begins shift".r

  val records = source.getLines.toList.sorted.map {
    case recordPattern(minute, text) => (minute.toInt, text)
  }
  source.close

  val schedules = {
    val (_, _, schedules) = records.foldLeft((-1, -1, List.empty[(Int, Int)])) {
      case ((guardId, fellAsleepAt, schedules), (currMinute, currText)) =>
        currText match {
          case "falls asleep" => (guardId, currMinute, schedules)
          case "wakes up" =>
            val minutesAsleep = fellAsleepAt until currMinute
            (guardId, -1, schedules ++ minutesAsleep.map(m => (guardId, m)))
          case guardPattern(newGuardId) => (newGuardId.toInt, -1, schedules)
        }
    }
    schedules
  }

  def solvePart1: Int = {
    val (guardId, _) = schedules.groupBy(_._1).maxBy(_._2.length)
    val (minute, _) = schedules
      .collect { case (gId, m) if gId == guardId => m }
      .groupBy(identity)
      .maxBy(_._2.length)
    guardId * minute
  }

  def solvePart2: Int = {
    val (guardId, minute) = schedules.groupBy(identity).maxBy(_._2.length)._1
    guardId * minute
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
