package twentytwentyone

import scala.io.Source

object Day08 extends App {
  val source = Source.fromResource("twentytwentyone/Day08.txt")
  val instructions = source.getLines.toList.map(
    _.split(""" \| """).map(_.split(" ").toList.map(_.toSet))
  )
  source.close

  def solvePart1: Long = instructions
    .map { case Array(signals, output) =>
      val lengths = List(2, 4, 3, 7)
      output.count(a => signals.filter(s => lengths.contains(s.size)).contains(a))
    }
    .sum

  case class Resolver(digit: Int, size: Int, intersects: Map[Int, Int] = Map.empty) {
    def matches(signal: Set[Char], acc: Map[Int, Set[Char]]): Boolean = {
      signal.size == this.size &&
        this.intersects.forall {
          case (digit, numCommon) => signal.intersect(acc(digit)).size == numCommon
        }
    }
  }

  // The order of the resolvers is important. If a number is
  // included in `intersects`, then the number must have been
  // found with a previous resolver.
  val resolvers: List[Resolver] = List(
    Resolver(1, size = 2),
    Resolver(4, size = 4),
    Resolver(7, size = 3),
    Resolver(8, size = 7),
    Resolver(6, size = 6, intersects = Map(1 -> 1, 8 -> 6)),
    Resolver(9, size = 6, intersects = Map(4 -> 4)),
    Resolver(0, size = 6, intersects = Map(1 -> 2, 4 -> 3)),
    Resolver(2, size = 5, intersects = Map(1 -> 1, 6 -> 4, 8 -> 5)),
    Resolver(5, size = 5, intersects = Map(1 -> 1, 6 -> 5, 8 -> 5)),
    Resolver(3, size = 5, intersects = Map(5 -> 4, 8 -> 5)),
  )

  def solvePart2: Long = instructions
    .map { case Array(signals, output) =>
      val digitToSignal = resolvers.foldLeft(Map.empty[Int, Set[Char]]) {
        case (digitToSignal, resolver) =>
          // There should only be one match.
          val (List(signal), rest) = signals.partition(s => resolver.matches(s, digitToSignal))
          digitToSignal + (resolver.digit -> signal)
      }
      val signalToDigit = digitToSignal.map(_.swap)
      Integer.parseInt(output.map(signalToDigit).mkString, 10)
    }
    .sum

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
