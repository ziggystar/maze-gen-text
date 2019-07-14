import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.alg.spanning.KruskalMinimumSpanningTree
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}
import org.jgrapht.{Graphs, UndirectedGraph}

import scala.collection.JavaConverters._
import scala.util.Random

package object laby {
  def wheelOfFortune[N](xs: List[N], random: Random = Random.self)(weight: N => Double): N = {
    def find(r: Double, s: List[N]): N = s match {
      case h :: t if weight(h) > r => h
      case h :: t => find(r - weight(h), t)
      case Nil => sys.error(s"oops: $xs; ${xs.map {weight}}")
    }
    find(random.nextDouble() * xs.foldLeft(0d)(_ + weight(_)),xs)
  }

  type UGraph[N] = UndirectedGraph[N,DefaultEdge]

  implicit class RichGraph[N](val g: UGraph[N]) extends AnyVal {
    def neighboursOf(n: N): Set[N] =
      g.edgesOf(n).asScala.map { e =>
        val n1 = g.getEdgeSource(e)
        if (n1 != n) n1 else g.getEdgeTarget(e)
      }(collection.breakOut)

    def nodes: Set[N] = g.vertexSet.asScala.toSet

    def edges: Set[BiSet[N]] =
      g.edgeSet().asScala.map{e => BiSet(g.getEdgeSource(e), g.getEdgeTarget(e))}(collection.breakOut)

    def subgraph(verts: Set[N]): UGraph[N] = {
      val r = new SimpleGraph[N,DefaultEdge](classOf[DefaultEdge])
      g.vertexSet().asScala.intersect(verts).foreach(r.addVertex)
      edges.filter(e => verts(e.n1) && verts(e.n2)).foreach(e => r.addEdge(e.n1, e.n2))
      r
    }

    def copy: UGraph[N] = {
      val r = new SimpleGraph[N,DefaultEdge](classOf[DefaultEdge])
      Graphs.addGraph(r,g)
      r
    }
  }

  def pathOfLength[N](_g: UGraph[N], start: N, end: N, length: Int, maxTries: Int = 10, random: Random = Random.self): Option[Seq[N]] = {
    val initialDistance: Double = {
      val d = new DijkstraShortestPath(_g)
      d.getPath(start,end).getLength.toDouble
    }
    def desiredDistance(pathLength: Int): Double = initialDistance - pathLength * (initialDistance/length) + 5
    def genOnePath() = {
      val g = _g.copy
      val r = Iterator.iterate(Option(List(start))){
        case Some(path@h::t) if h != end =>
          val neighbours = g.neighboursOf(h)
          g.removeVertex(h)
          val dijkstra = new DijkstraShortestPath(g)
          //calculate the shortest path from each neighbour to the destination and filter out too long paths
          val candidates: Set[(N, Int)] = neighbours
            .flatMap(n => Option(dijkstra.getPath(n, end)).map(n -> _.getLength))
            .filter(_._2 <= length - path.size)
          //draw a random candidate
          Option(candidates)
            .filter(_.nonEmpty)
            .map(cand => wheelOfFortune(cand.toList) { case (_, l) =>
              val r = math.exp(math.pow(l - desiredDistance(path.length), 2) * -1)
              r
            }._1)
            .map(_ :: path)
        case _ => None
      }.takeWhile(_.isDefined).toSeq.last.get.reverse
      r
    }
    Iterator.continually(genOnePath()).take(maxTries).find(_.length >= length)
  }

  def spanningTrees[N](g: UGraph[N]): Set[BiSet[N]] = {
    val k = new KruskalMinimumSpanningTree(g)
    k.getSpanningTree.getEdges.asScala.toSet.map((e: DefaultEdge) => BiSet(g.getEdgeSource(e),g.getEdgeTarget(e)))
  }

}
