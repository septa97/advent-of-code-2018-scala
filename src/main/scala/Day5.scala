import scala.io.Source

object Day5 {
  private def hasReaction(a: Char, b: Char): Boolean =
    a != b && a.toLower == b.toLower

  private def react(input: String): String = {
    input.foldLeft("")((result, curr) => {
      if (!result.isEmpty && hasReaction(result.last, curr))
        result.dropRight(1)
      else
        result + curr
    })
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input/day5/input.txt").mkString.trim
    val result = react(input)

    println(s"part 1: ${result.length}")

    val uniqueChars = input.map(_.toLower).toSet
    val ans = uniqueChars.map(char =>
      react(input.filter(c => c != char && c != char.toUpper)).length)

    println(s"part 2: ${ans.min}")
  }
}
