package twentytwentyone

import scala.io.Source

object Day13 extends App {
  val source = Source.fromResource("twentytwentyone/Day13.txt")
  val instructions = source.getLines.toList
  source.close

  type Grid = Seq[Seq[Boolean]]

  val (folds, strPoints) = instructions
    .filter(_.strip != "")
    .partition(_.contains("fold"))

  val points = strPoints
    .map(_.split(","))
    .map {
      case Array(strX, strY) => (strX.toInt, strY.toInt)
    }
    .toSet

  val maxX = points.maxBy(_._1)._1
  val maxY = points.maxBy(_._2)._2

  val grid: Grid = (0 to maxY).map(y => {
    (0 to maxX).map(x => points.contains((x, y)))
  })

  def printGrid(grid: Grid): Unit = grid
    .map(_.map(cell => if (cell) "#" else " ").mkString(""))
    .foreach(println)

  def foldUp(grid: Grid, n: Int): Grid = {
    val top = grid.take(n)
    val bottom = grid.drop(n + 1).reverse

    val List(smallest, largest) = List(top, bottom).sortBy(_.size)
    val diff = largest.length - smallest.length
    val topRows = largest.take(diff)
    val bottomRows = largest.drop(diff).zipWithIndex.map {
      case (row, i) => row.zip(smallest(i)).map { case (a, b) => a || b }
    }
    topRows ++ bottomRows
  }

  def foldLeft(grid: Grid, n: Int): Grid =
    foldUp(grid.transpose, n).transpose

  val foldPattern = "fold along (?<axis>[xy])=(?<n>\\d+)".r

  def solve(folds: List[String]): Grid = folds.foldLeft(grid) {
    case (acc, curr) =>
      curr match {
        case foldPattern("x", n) => foldLeft(acc, n.toInt)
        case foldPattern("y", n) => foldUp(acc, n.toInt)
      }
  }

  println("Part 1: " + solve(folds.take(1)).map(_.count(identity)).sum)
  println("Part 2: ")
  printGrid(solve(folds))
}
