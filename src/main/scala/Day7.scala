import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day7 {
  private def sortNodes(
      s: mutable.SortedSet[String],
      adjList: mutable.Map[String, List[String]],
      incomingEdgesMap: mutable.Map[String, Int]): List[String] = {
    val sortedNodes: ListBuffer[String] = new ListBuffer[String]()

    while (s.nonEmpty) {
      val n = s.head
      s -= s.head
      sortedNodes += n

      adjList.get(n).foreach { neighbors =>
        neighbors.foreach { m =>
          incomingEdgesMap
            .get(m)
            .map { e =>
              if (e - 1 == 0) s += m

              e - 1
            }
            .foreach(e => incomingEdgesMap += m -> e)
        }
      }
    }

    sortedNodes.toList
  }

  def main(args: Array[String]): Unit = {
    val edges = Source.fromFile("input/day7/input.txt").getLines().map { line =>
      val pattern =
        "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
      val pattern(from, to) = line
      from -> to
    }

    val nodes = new ListBuffer[String]()
    val adjList = mutable.Map[String, List[String]]()
    val incomingEdgesMap = mutable.Map[String, Int]()

    for {
      (k, v) <- edges
    } {
      val newValues = adjList.get(k) match {
        case Some(values) => (values.to[ListBuffer] += v).toList
        case None         => List(v)
      }

      adjList += k -> newValues
      nodes += k
      nodes += v

      val incomingEdgesValue = incomingEdgesMap.get(v) match {
        case Some(values) => values + 1
        case None         => 1
      }

      incomingEdgesMap += v -> incomingEdgesValue
    }

    val s = nodes.to[mutable.SortedSet]

    for ((_, v) <- adjList) v.foreach(e => s -= e)

    val sortedNodes = sortNodes(s, adjList, incomingEdgesMap).mkString
    println(s"part 1: $sortedNodes")
  }
}
