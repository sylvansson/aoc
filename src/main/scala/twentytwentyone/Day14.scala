package twentytwentyone

import scala.collection.mutable
import scala.io.Source

object Day14 extends App {
  val source = Source.fromResource("twentytwentyone/Day14.txt")
  val instructions = source.getLines.toList
  source.close

  val template = instructions.head

  val rules = instructions
    .drop(2)
    .map(_.split(" -> "))
    .map {
      case Array(pair, insert) => pair -> pair.patch(1, insert, 0)
    }
    .toMap

  def solve(steps: Int): Long = {
    var countByPair = template
      .sliding(2)
      .toList
      .groupBy(identity)
      .view
      .mapValues(_.size.toLong)
      .to(mutable.Map)

    for (_ <- 0 until steps) {
      val tempCounts = mutable.Map.empty[String, Long].withDefaultValue(0L)
      for ((pair, count) <- countByPair) {
        for (p <- rules(pair).sliding(2)) {
          tempCounts(p) += count
        }
      }
      countByPair = tempCounts
    }
    countByPair("B") = 1

    val countByChar = countByPair.groupBy(_._1.head).view.mapValues(_.values.sum)
    val sortedCounts = countByChar.values.toList.sorted
    sortedCounts.last - sortedCounts.head
  }

  println("Part 1: " + solve(10))
  println("Part 2: " + solve(40))
}
