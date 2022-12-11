package twentytwentytwo

import scala.io.Source

object Day08 extends App {
  val source = Source.fromResource("twentytwentytwo/Day08.txt")
  val trees = source.getLines.toList.map(_.split("").map(_.toInt).toList)
  source.close

  val width = trees.head.length
  val height = trees.length

  val allPositions = for (y <- 0 until height; x <- 0 until width) yield (x, y)

  def getTree(pos: (Int, Int)) = trees(pos._2)(pos._1)

  def getAdjacentTrees(pos: (Int, Int)): List[List[Int]] = getAdjacentTrees(pos._1, pos._2)
  def getAdjacentTrees(x: Int, y: Int): List[List[Int]] =
    List(
      (0 until x).map(_ -> y).reverse,
      (x + 1 until width).map(_ -> y),
      (0 until y).map(x -> _).reverse,
      (y + 1 until height).map(x -> _),
    )
      .map(_.map(getTree).toList)

  def isVisible(pos: (Int, Int)): Boolean =
    getAdjacentTrees(pos).exists(_.forall(_ < getTree(pos)))

  def scenicScore(pos: (Int, Int)): Int =
    getAdjacentTrees(pos)
      .map(d => {
        val i = d.indexWhere(_ >= getTree(pos))
        if (i == -1) d.length else i + 1
      })
      .product

  def solvePart1: Long = allPositions.count(isVisible)

  def solvePart2: Long = allPositions.map(scenicScore).max

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
