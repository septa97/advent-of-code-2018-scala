import scala.io.Source

object Day8 {
  case class Node(length: Int, sum: Int)

  def accumulate(arr: Array[Int], start: Int): Node = {
    val children = arr(start)
    val metadataEntries = arr(start + 1)

    if (children > 0) {
      var totalLength = start + 2

      val total = for {
        _ <- 0 until children
      } yield {
        val temp = accumulate(arr, totalLength)
        totalLength += temp.length
        temp
      }

      val nodeLength = total.map(_.length).sum + metadataEntries + 2
      val nodeTotal = total.map(_.sum).sum + arr
        .slice(totalLength, totalLength + metadataEntries)
        .sum

      Node(nodeLength, nodeTotal)
    } else {
      val metadataStart = start + 2
      val metadataEnd = start + 2 + metadataEntries
      Node(metadataEntries + 2, arr.slice(metadataStart, metadataEnd).sum)
    }
  }

  def accumulateBasedOnMetadata(arr: Array[Int], start: Int): Node = {
    val children = arr(start)
    val metadataEntries = arr(start + 1)

    if (children > 0) {
      var totalLength = start + 2

      val total = for {
        _ <- 0 until children
      } yield {
        val temp = accumulateBasedOnMetadata(arr, totalLength)
        totalLength += temp.length
        temp
      }

      val nodeLength = total.map(_.length).sum + metadataEntries + 2
      val nodeTotal = arr
        .slice(totalLength, totalLength + metadataEntries)
        .map(e => if (e - 1 < total.length) total(e - 1).sum else 0)
        .sum

      Node(nodeLength, nodeTotal)
    } else {
      val metadataStart = start + 2
      val metadataEnd = start + 2 + metadataEntries
      Node(metadataEntries + 2, arr.slice(metadataStart, metadataEnd).sum)
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Source
      .fromFile("input/day8/input.txt")
      .mkString
      .split("\\s+")
      .map(_.toInt)

    println(s"part 1: ${accumulate(arr, 0).sum}")
    println(s"part 2: ${accumulateBasedOnMetadata(arr, 0).sum}")
  }
}
