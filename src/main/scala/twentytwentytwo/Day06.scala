package twentytwentytwo

import scala.io.Source

object Day06 extends App {
  val source = Source.fromResource("twentytwentytwo/Day06.txt")
  val instructions = source.getLines.toList.head
  source.close

  def getMarkerEndPosition(n: Int): Int = instructions
    .sliding(n)
    .zipWithIndex
    .collectFirst {
      case (seq, i) if seq.toSet.size == n => i + n
    }
    .head

  println("Part 1: " + getMarkerEndPosition(4))
  println("Part 2: " + getMarkerEndPosition(14))
}
