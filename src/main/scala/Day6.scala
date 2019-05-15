import scala.io.Source

object Day6 {
  final case class Point(x: Int, y: Int)

  private def isAtEdge(i: Int, j: Int, height: Int, width: Int): Boolean = {
    i == 0 || i == height || j == 0 || j == width
  }

  private def distance(a: Point, b: Point): Int = {
    math.abs(a.x - b.x) + math.abs(a.y - b.y)
  }

  def main(args: Array[String]): Unit = {
    val pointMap =
      Source
        .fromFile("input/day6/input.txt")
        .getLines()
        .toList
        .map(_.split(", "))
        .zipWithIndex
        .map {
          case (Array(x, y), idx) => idx -> Point(x.toInt, y.toInt)
        }
        .toMap

    val width = pointMap.values.maxBy(_.x).x
    val height = pointMap.values.maxBy(_.y).y

    val board = for {
      i <- 0 to height
      j <- 0 to width
    } yield {
      val distanceMap = pointMap.map {
        case (id, p) => id -> distance(p, Point(j, i))
      }

      val minDist = distanceMap.minBy(_._2)

      if (distanceMap.values.count(_ == minDist._2) > 1) -1
      else minDist._1
    }

    val finalDist = board.zipWithIndex
      .map {
        case (id, idx) =>
          (id, (idx / (width + 1), idx % (width + 1)))
      }
      .groupBy(_._1)
      .values
      .map(_.map {
        case (_, (i, j)) => isAtEdge(i, j, height, width)
      })
      .map(e => if (e.exists(identity)) -1 else e.length)

    println(s"part 1: ${finalDist.max}")

    val regions = for {
      i <- 0 to height
      j <- 0 to width
    } yield {
      pointMap.map {
        case (_, p) => distance(p, Point(j, i))
      }.sum
    }

    println(s"part 2: ${regions.count(_ < 10000)}")
  }
}
