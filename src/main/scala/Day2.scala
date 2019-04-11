import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    val counts = for {
      line <- Source.fromFile("input/day2/part1.txt").getLines().toList
      charCount = line.groupBy(identity).mapValues(_.length)
    } yield charCount.exists(_._2 == 2) -> charCount.exists(_._2 == 3)

    println(
      s"${counts.map(_._1).count(identity) * counts.map(_._2).count(identity)}")
  }
}
