package twentytwentyone

import scala.io.Source

object Day12 extends App {
  val source = Source.fromResource("twentytwentyone/Day12.txt")
  val instructions = source.getLines.toList.map(_.split("-") match {
    case Array(source, target) => (source, target)
  })
  source.close

  val (start, end) = ("start", "end")

  val targetsBySource = instructions
    .flatMap {
      case (source, target) => List((source, target), (target, source))
    }
    .filterNot {
      case (source, target) => source == end || target == start
    }
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2))
    .toMap

  def isSmall(id: String) = id.forall(_.isLower)

  def solvePart1: Long = {
    def getPaths(source: String, seen: Set[String]): List[List[String]] = {
      val newSeen = if (isSmall(source)) seen + source else seen

      targetsBySource.get(source) match {
        case Some(targets) =>
          for {
            target <- targets if !isSmall(target) || !seen.contains(target)
            path <- getPaths(target, newSeen)
          } yield {
            source +: path
          }
        case None => List(List(source))
      }
    }

    getPaths(start, Set.empty).size
  }

  def solvePart2: Long = {
    def getPaths(source: String, seenOnce: Set[String], seenTwice: Option[String]): List[List[String]] = {
      val newSeenTwice = if (isSmall(source)) {
        if (seenTwice.isDefined) seenTwice
        else if (seenOnce.contains(source)) Some(source)
        else seenTwice
      } else {
        seenTwice
      }
      val newSeenOnce = if (isSmall(source)) seenOnce + source else seenOnce

      targetsBySource.get(source) match {
        case Some(targets) =>
          for {
            target <- targets if !isSmall(target) || !newSeenOnce.contains(target) || newSeenTwice.isEmpty
            path <- getPaths(target, newSeenOnce, newSeenTwice)
          } yield {
            source +: path
          }
        case None => List(List(source))
      }
    }

    getPaths(start, Set.empty, None).size
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
