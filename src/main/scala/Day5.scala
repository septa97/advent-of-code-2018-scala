import scala.io.Source

object Day5 {
  def main(args: Array[String]): Unit = {
    var input = Source.fromFile("input/day5/input.txt").mkString.trim

    var i = 0
    var j = i + 1

    while (i < input.length - 1) {
      if (input(i).toLower == input(j).toLower && input(i) != input(j)) {
        input = input.substring(0, i) + input.substring(j + 1)

        if (i - 1 >= 0) i -= 1
        else i = 0

        j = i + 1
      } else {
        i += 1
        j = i + 1
      }
    }

    println(s"part 1: ${input.length}")
  }
}
