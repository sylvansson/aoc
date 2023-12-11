package twentytwentythree

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App {
  val source = Source.fromResource("twentytwentythree/Day08.txt")
  val instructions = source.getLines.toList
  source.close

  val mapPattern = "(?<from>\\w{3})\\s*=\\s*\\((?<left>\\w{3}), (?<right>\\w{3})\\)".r

  val directions = instructions.head.toCharArray.toList
  val (leftOf, rightOf) = instructions.drop(2).foldLeft((
    Map.empty[String, String],
    Map.empty[String, String],
  )) {
    case ((leftOf, rightOf), mapPattern(from, left, right)) =>
      (leftOf + (from -> left), rightOf + (from -> right))
  }

  def getNextNode(dir: Char, currNode: String): String =
    if (dir == 'L') leftOf(currNode) else rightOf(currNode)

  def solve(startNode: String, endNodeSuffix: String): Long = {
    @tailrec
    def loop(remainingDirs: List[Char], currNode: String, numSteps: Int): Int = {
      if (currNode.endsWith(endNodeSuffix)) {
        return numSteps
      }

      val dir :: rest = remainingDirs
      loop(
        if (rest.isEmpty) directions else rest,
        getNextNode(dir, currNode),
        numSteps + 1
      )
    }

    loop(directions, startNode, 0)
  }

  def leastCommonMultiple(nums: List[Long]): Long = {
    val largest = nums.max
    for (m <- LazyList.from(1).map(_ * largest)) {
      if (nums.forall(n => m % n == 0)) {
        return m
      }
    }
    // This shouldn't happen.
    -1
  }

  def solvePart1: Long = solve("AAA", "ZZZ")

  def solvePart2: Long = {
    val startNodes = (leftOf ++ rightOf).keys.filter(_.endsWith("A")).toList
    leastCommonMultiple(startNodes.map(solve(_, "Z")))
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
