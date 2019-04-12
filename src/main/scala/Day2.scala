import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {
    val counts = for {
      line <- Source.fromFile("input/day2/input.txt").getLines().toList
      charCount = line.groupBy(identity).mapValues(_.length)
    } yield charCount.exists(_._2 == 2) -> charCount.exists(_._2 == 3)

    println(
      s"part 1: ${counts.map(_._1).count(identity) * counts.map(_._2).count(identity)}")

    val lines = Source.fromFile("input/day2/input.txt").getLines().toList
    val diffs = for {
      i <- lines.indices
      j <- i + 1 until lines.length
      zipped = lines(i).zip(lines(j))
    } yield
      zipped.filter(e => e._1 == e._2).map(_._1) -> zipped.count(e =>
        e._1 != e._2)

    println(s"part 2: ${diffs.minBy(_._2)._1.mkString}")
  }
}
