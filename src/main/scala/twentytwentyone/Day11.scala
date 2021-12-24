package twentytwentyone

import scala.io.Source

object Day11 extends App {
  val source = Source.fromResource("twentytwentyone/Day11.txt")
  val instructions = source.getLines.toList.map(_.split("").map(a => a.toInt))
  source.close

  val width = 10
  val range = 0 until width

  val initialOctos = instructions.flatten

  def toIndex(x: Int, y: Int): Int =
    y * width + x
  def fromIndex(i: Int): (Int, Int) =
    (i % width, i / width)

  def solve(part: Int): Long = {
    var octos = initialOctos
    var count = 0
    val steps = if (part == 1) 100 else 10000

    for (i <- 1 to steps) {
      var tempOctos = octos.map(_ + 1)

      var newFlashes = tempOctos.zipWithIndex.filter(_._1 > 9)
      while (newFlashes.nonEmpty) {
        val increaseByIndex = newFlashes
          .flatMap { case (_, i) =>
            val (x, y) = fromIndex(i)
            List(
              (x, y - 1), // Top.
              (x + 1, y - 1), // Top right.
              (x + 1, y), // Right.
              (x + 1, y + 1), // Bottom right.
              (x, y + 1), // Bottom.
              (x - 1, y + 1), // Bottom left.
              (x - 1, y), // Left.
              (x - 1, y - 1), // Top left.
            )
              .collect {
                case (x, y) if range.contains(x) && range.contains(y) =>
                  (tempOctos(toIndex(x, y)), toIndex(x, y))
              }
          }
          .groupBy(_._2) // By index.
          .view
          .mapValues(_.size)
          .toMap

        // Octopuses with their energy level, along with whether or
        // not the octopus just started flashing.
        val updatedOctos = tempOctos.zipWithIndex.map { case (octo, i) =>
          val increase = increaseByIndex.getOrElse(i, 0)
          ((octo + increase, i), octo <= 9 && octo + increase > 9)
        }
        newFlashes = updatedOctos.collect {
          case (octo, isNewFlash) if isNewFlash => octo
        }
        tempOctos = updatedOctos.map(_._1._1)
      }

      // Set energy level back to 0 if applicable.
      octos = tempOctos.map(octo => if (octo > 9) 0 else octo)
      // Count the octopuses that flashed.
      count += tempOctos.count(_ > 9)

      // The octopuses are in sync.
      if (part == 2 && octos.forall(_ == 0)) {
        return i
      }
    }

    count
  }

  println("Part 1: " + solve(part = 1))
  println("Part 2: " + solve(part = 2))
}
