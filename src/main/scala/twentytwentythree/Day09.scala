package twentytwentythree

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends App {
  val source = Source.fromResource("twentytwentythree/Day09.txt")
  val instructions = source.getLines.toList.map(_.split(" ").map(_.toLong).toList)
  source.close

  @tailrec
  def getSequences(seqs: List[List[Long]]): List[List[Long]] = {
    if (seqs.last.forall(_ == 0)) {
      return seqs
    }

    val nextSeq = seqs
      .last
      .sliding(2)
      .map { case List(a, b) => b - a }
      .toList
    getSequences(seqs :+ nextSeq)
  }

  def solvePart1: Long = instructions
    .map(insts => getSequences(List(insts)).map(_.last).sum)
    .sum

  def solvePart2: Long = instructions
    .map(insts =>
      getSequences(List(insts))
        .map(_.head)
        .reverse
        .reduceLeft((a, b) => b - a))
    .sum

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
