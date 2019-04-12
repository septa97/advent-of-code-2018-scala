import scala.io.Source

object Day3 {
  final case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  def main(args: Array[String]): Unit = {
    val claims = for {
      line <- Source.fromFile("input/day3/input.txt").getLines().toList
      Array(idStr, _, leftTop, widthHeight) = line.split(' ')
      id = idStr.trim.drop(1)
      Array(left, top) = leftTop.dropRight(1).trim.split(',')
      Array(width, height) = widthHeight.trim.split('x')
    } yield
      Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt) -> true

    val WIDTH = {
      val claimMaxWidth = claims.maxBy(e => e._1.left + e._1.width)
      claimMaxWidth._1.left + claimMaxWidth._1.width
    }
    val HEIGHT = {
      val claimMaxHeight = claims.maxBy(e => e._1.top + e._1.height)
      claimMaxHeight._1.top + claimMaxHeight._1.height
    }
    val matrix = Array.ofDim[Int](WIDTH, HEIGHT)

    for {
      claim <- claims
      i <- claim._1.left until (claim._1.left + claim._1.width)
      j <- claim._1.top until (claim._1.top + claim._1.height)
    } {
      matrix(i)(j) += 1
    }

    println(s"part 1: ${matrix.flatten.count(_ >= 2)}")
  }
}
