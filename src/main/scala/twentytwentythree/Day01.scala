package twentytwentythree

import scala.io.Source

object Day01 extends App {
  val source = Source.fromResource("twentytwentythree/Day01.txt")
  val instructions = source.getLines.toList
  source.close

  val spelledOutDigits = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
  )

  def solvePart1: Long = instructions
    .map { inst =>
      val digits = inst.filter(_.isDigit)
      (digits.head.toString + digits.last).toInt
    }
    .sum

  def solvePart2: Long = {
    val validDigits = spelledOutDigits.keys ++ spelledOutDigits.values

    def toSpelledOut(digit: String): String =
      spelledOutDigits.getOrElse(digit, digit)

    instructions
      .map { inst =>
        val digitsWithIndexes = validDigits.collect {
          case d if inst.contains(d) =>
            (toSpelledOut(d), inst.indexOf(d), inst.lastIndexOf(d))
        }
        val first = digitsWithIndexes.minBy(_._2)._1
        val last = digitsWithIndexes.maxBy(_._3)._1
        (first + last).toInt
      }
      .sum
  }

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
