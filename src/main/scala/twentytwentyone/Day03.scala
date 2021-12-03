package twentytwentyone

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {
  val source = Source.fromResource("twentytwentyone/Day03.txt")
  val instructions = source.getLines.toList.map(_.toList.map(_.asDigit))
  source.close

  def solvePart1: Long = {
    val columns = instructions.head.indices.map(i => instructions.map(_(i)))

    def getRate(implicit ord: Ordering[Int]): Int = {
      val bin = columns.map(column =>
        column.minBy(bit => column.count(_ == bit))(ord)
      )
      Integer.parseInt(bin.mkString(""), 2)
    }
    getRate * getRate(Ordering.Int.reverse)
  }

  def solvePart2: Long = {
    @tailrec
    def getRating(instructions: List[List[Int]], i: Int)(implicit ord: Ordering[(Int, Int)]): Long = {
      instructions match {
        case bin :: Nil =>
          Integer.parseInt(bin.mkString(""), 2)
        case _ =>
          val column = instructions.map(_(i))
          val criteria = column.minBy(bit => (column.count(_ == bit), bit))(ord)

          val filtered = instructions.filter(_(i) == criteria)
          getRating(filtered, i + 1)
      }
    }
    getRating(instructions, 0) *
      getRating(instructions, 0)(Ordering.Tuple2[Int, Int].reverse)
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
