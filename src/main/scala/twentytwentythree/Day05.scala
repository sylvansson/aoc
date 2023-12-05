package twentytwentythree

import utils.ExtractInt
import utils.ExtractLong

import scala.io.Source

object Day05 extends App {
  val source = Source.fromResource("twentytwentythree/Day05.txt")
  val instructions = source.getLines.toList
  source.close

  val seeds :: maps = instructions.foldLeft(List(List.empty[String])) {
    case (acc, curr) if curr.trim.isEmpty =>
      acc :+ Nil
    case (acc, curr) =>
      acc.init :+ (acc.last :+ curr)
  }

  val seedNumbers: Array[Long] = seeds
    .head
    .split(": ")
    .apply(1)
    .split(" ")
    .map(_.toLong)

  val mappings = maps.map(
    _.drop(1).map(
      _.split(" ") match {
        case Array(ExtractLong(dest), ExtractLong(source), ExtractInt(length)) =>
          Mapping(source, dest, length)
      }
    )
  )

  case class Mapping(source: Long, dest: Long, length: Int) {
    def resolve(x: Long): Long =
      dest + (x - source)
    def contains(x: Long): Boolean =
      x >= source && x <= (source + length - 1)
  }

  def solvePart1: Long = seedNumbers
    .map(seed =>
      mappings.foldLeft(seed) {
        case (acc, curr) => curr
          .flatMap(r => Option.when(r.contains(acc))(r.resolve(acc)))
          .headOption
          .getOrElse(acc)
      }
    )
    .min

  // This runs in approximately 8 minutes on my machine.
  def solvePart2: Long = {
    val seeds = seedNumbers.grouped(2).flatMap {
      case Array(start, length) => start until start + length
    }

    seeds.foldLeft(Long.MaxValue) {
      case (minLocation, curr) =>
        val location = mappings.foldLeft(curr) {
          case (acc, ranges) => ranges
            .collectFirst {
              case r if r.contains(acc) => r.resolve(acc)
            }
            .getOrElse(acc)
        }
        minLocation.min(location)
    }
  }

  println("Part 1: " + solvePart1)
  val startTime = System.currentTimeMillis
  println("Part 2: " + solvePart2)
  println(s"Time for part 2: ${System.currentTimeMillis - startTime} ms")
}
