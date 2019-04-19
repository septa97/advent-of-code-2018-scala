import scala.collection.mutable
import scala.io.Source

object Day4 {
  final case class Detail(date: String, time: String, action: String)
      extends Ordered[Detail] {
    import scala.math.Ordered.orderingToOrdered

    override def compare(that: Detail): Int =
      (this.date, this.time, this.action) compare (that.date, that.time, that.action)
  }

  private def getSleepCount(details: List[Detail]): Array[Int] = {
    val possiblyIncompleteSleepArray = details
      .filter(e => e.action == "falls asleep" || e.action == "wakes up")
      .sortWith((a, b) => a.time.compareTo(b.time) < 0)
      .sliding(2)
      .zipWithIndex
      .flatMap {
        case (List(Detail(_, prevTime, _), Detail(_, currTime, currAction)), 0)
            if currAction == "wakes up" =>
          val a = prevTime.substring(3).toInt
          val b = currTime.substring(3).toInt
          val size = b - a
          Array.fill[Int](a - 0)(0) ++ Array.fill[Int](size)(1) :+ 0
        case (List(Detail(_, prevTime, _), Detail(_, currTime, currAction)), _)
            if currAction == "falls asleep" =>
          val a = prevTime.substring(3).toInt
          val b = currTime.substring(3).toInt
          val size = b - a - 1
          Array.fill[Int](size)(0) :+ 1
        case (List(Detail(_, prevTime, _), Detail(_, currTime, currAction)), _)
            if currAction == "wakes up" =>
          val a = prevTime.substring(3).toInt
          val b = currTime.substring(3).toInt
          val size = b - a - 1
          Array.fill[Int](size)(1) :+ 0
      }
      .toArray

    val sleepArray = possiblyIncompleteSleepArray ++ Array.fill[Int](
      60 - possiblyIncompleteSleepArray.length)(
      if (!possiblyIncompleteSleepArray.isEmpty)
        possiblyIncompleteSleepArray.last
      else 0)

    assert(sleepArray.length == 60)

    sleepArray
  }

  def main(args: Array[String]): Unit = {
    val details = for {
      line <- Source.fromFile("input/day4/input.txt").getLines().toList
      Array(d, action) = line.split("] ")
      datetime = d.drop(1).split(' ')
    } yield Detail(datetime(0), datetime(1), action.trim)

    val sortedDetails = details.sorted

    val shiftStarts = sortedDetails
      .map(_.action.startsWith("Guard #"))
      .zipWithIndex
      .filter(_._1)
      .map(_._2)

    val sleepCount = mutable.Map[String, Array[Int]]()

    for {
      i <- 0 until shiftStarts.length - 1
      j = i + 1
    } {
      val guardId = sortedDetails(shiftStarts(i)).action.split(' ')(1).drop(1)
      val details = sortedDetails.slice(shiftStarts(i) + 1, shiftStarts(j))

      if (sleepCount.contains(guardId)) {
        sleepCount(guardId) = sleepCount
          .getOrElse(guardId, Array.ofDim[Int](60))
          .zip(getSleepCount(details))
          .map(e => e._1 + e._2)
      } else {
        sleepCount += guardId -> getSleepCount(details)
      }
    }

    val part1GuardMax = sleepCount.maxBy(_._2.sum)
    println(
      s"part 1: ${part1GuardMax._1.toInt * part1GuardMax._2.zipWithIndex.maxBy(_._1)._2}")

    val part2GuardMax = sleepCount.maxBy(_._2.max)
    println(
      s"part 2: ${part2GuardMax._1.toInt * part2GuardMax._2.zipWithIndex.maxBy(_._1)._2}")
  }
}
