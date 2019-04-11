import scala.collection.mutable
import scala.io.Source

object Day1 {
  def main(args: Array[String]) = {
    val arr =
      Source.fromFile("input/day1/input.txt").getLines().toList.map(_.toInt)

    println(s"part 1: ${arr.sum}")

    var currFreq = 0
    var i = 0
    val freqs = mutable.Set[Int]()

    while (freqs.add(currFreq)) {
      currFreq += arr(i)

      if (i + 1 >= arr.length) i = 0
      else i += 1
    }

    println(s"part 2: $currFreq")
  }
}
