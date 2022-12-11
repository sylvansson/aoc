package twentytwentytwo

import scala.io.Source

object Day07 extends App {
  val source = Source.fromResource("twentytwentytwo/Day07.txt")
  val instructions = source.getLines.toList
  source.close

  val cdPattern = "\\$ cd (?<path>.+)".r

  val groupedInstructions = instructions.foldLeft(List.empty[List[String]]) {
    case (acc, curr) if curr.startsWith("$") => acc :+ List(curr)
    // We don't care about directories listed by `ls`.
    case (acc, curr) if curr.startsWith("dir") => acc
    case (acc, curr) => acc.init :+ (acc.lastOption.getOrElse(Nil) :+ curr)
  }

  val emptyStack = List.empty[String]
  val emptySizes = Map.empty[List[String], Long].withDefaultValue(0L)

  val (_, sizes) = groupedInstructions.foldLeft(emptyStack, emptySizes) {
    case ((stack, sizes), cdPattern(dest) :: _) =>
      // Pop the last subfolder off the stack.
      if (dest == "..") (stack.dropRight(1), sizes)
      // Reset the stack.
      else if (dest == "/") (List("/"), sizes)
      // Add the subfolder to the stack.
      else (stack :+ dest, sizes)
    case ((stack, sizes), _ :: files) =>
      // Get the total size for the files in the current subfolder.
      val sum = files.map(_.split(" ").head.toLong).sum
      // Increment the size of each folder in the stack.
      val newValues = stack.indices
        .map(_ + 1)
        .map(stack.take)
        .map(s => s -> (sizes(s) + sum))
      (stack, sizes ++ newValues)
  }

  def solvePart1: Long = sizes.values.filter(_ < 100000).sum

  def solvePart2: Long = sizes.values.filter(_ > 30000000 - (70000000 - sizes(List("/")))).min

  println("Part 1: " + solvePart1)
  println("Part 2: " + solvePart2)
}
