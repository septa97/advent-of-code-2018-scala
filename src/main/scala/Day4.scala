import scala.collection.mutable
import scala.io.Source

object Day4 {
  final case class Detail(date: String, time: String, action: String)

  private def getSleepCount(details: List[Detail]): Array[Int] = {
    val shiftArrayNum = Array.ofDim[Int](60)

    for {
      detail <- details.sortWith((a, b) => a.time.compareTo(b.time) < 0)
    } {
      detail match {
        case Detail(_, time, action) if action == "falls asleep" =>
          val minute = time.substring(3).toInt
          shiftArrayNum(minute) = 1
        case Detail(_, time, action) if action == "wakes up" =>
          val minute = time.substring(3).toInt
          shiftArrayNum(minute) = 2
      }
    }

    var num = shiftArrayNum(0)

    val sleepArray = for {
      i <- shiftArrayNum.indices
    } yield {
      num match {
        case 0 if shiftArrayNum(i) != 1 =>
          0
        case 0 if shiftArrayNum(i) == 1 =>
          num = 1
          1
        case 1 if shiftArrayNum(i) != 2 =>
          1
        case 1 if shiftArrayNum(i) == 2 =>
          num = 2
          0
        case 2 if shiftArrayNum(i) != 1 =>
          0
        case 2 if shiftArrayNum(i) == 1 =>
          num = 1
          1
      }
    }

    sleepArray.toArray
  }

  def main(args: Array[String]): Unit = {
    val details = for {
      line <- Source.fromFile("input/day4/input.txt").getLines().toList
      Array(d, action) = line.split("] ")
      datetime = d.drop(1).split(' ')
    } yield Detail(datetime(0), datetime(1), action.trim)

    val sortedDetails = details
      .sortWith { (a, b) =>
        a.date.compareTo(b.date) match {
          case 0 =>
            a.time.compareTo(b.time) < 0
          case result =>
            result < 0
        }
      }

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
