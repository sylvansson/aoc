package twentyeighteen

import scala.io.Source

object Day03 extends App {
  val source = Source.fromResource("twentyeighteen/Day03.txt")
  val pattern = "#(?<id>\\d+) @ (?<x>\\d+),(?<y>\\d+): (?<width>\\d+)x(?<height>\\d+)".r

  case class Instruction(id: Int, x: Int, y: Int, width: Int, height: Int) {
    def getPoints: IndexedSeq[(Int, Int)] =
      for {
        x <- this.x until (this.x + width)
        y <- this.y until (this.y + height)
      } yield (x, y)
  }

  val instructions = source.getLines.toList.map {
    case pattern(id, x, y, width, height) => Instruction(
      id.toInt,
      x.toInt,
      y.toInt,
      width.toInt,
      height.toInt,
    )
  }
  source.close

  def solvePart1: Int =
    instructions.flatMap(_.getPoints).groupBy(identity).count(_._2.length >= 2)

  def solvePart2: Int = {
    val noOverlap = instructions.flatMap(_.getPoints).groupBy(identity).filter(_._2.length == 1)
    instructions
      .find(_.getPoints.forall(noOverlap.contains))
      .map(_.id)
      .get
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
