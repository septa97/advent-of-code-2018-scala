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
    } yield Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)

    val WIDTH = {
      val claimMaxWidth = claims.maxBy(e => e.left + e.width)
      claimMaxWidth.left + claimMaxWidth.width
    }
    val HEIGHT = {
      val claimMaxHeight = claims.maxBy(e => e.top + e.height)
      claimMaxHeight.top + claimMaxHeight.height
    }
    val matrix = Array.ofDim[Int](WIDTH, HEIGHT)

    for {
      claim <- claims
      i <- claim.left until (claim.left + claim.width)
      j <- claim.top until (claim.top + claim.height)
    } {
      matrix(i)(j) += 1
    }

    println(s"part 1: ${matrix.flatten.count(_ >= 2)}")

    val claimsWithBool = for {
      claim <- claims
      rows = matrix.slice(claim.left, claim.left + claim.width)
      isValid = rows
        .map(_.slice(claim.top, claim.top + claim.height).forall(_ == 1))
        .forall(identity)
    } yield claim -> isValid

    // Use get since we're sure that there's an answer
    println(s"part 2: ${claimsWithBool.find(_._2).get._1.id}")
  }
}
