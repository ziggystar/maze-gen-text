package laby

import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

trait Topology {
  type N

  def nodes: Set[N]

  def neighbours(n: N): Set[N]

  def allEdges: Set[BiSet[N]] = nodes.flatMap(n => neighbours(n).map(nn => BiSet(n,nn)))

  def graph: UGraph[N] = {
    val g = new SimpleGraph[N,DefaultEdge](classOf[DefaultEdge])
    nodes.foreach(g.addVertex)
    for{
      v <- nodes
      n <- neighbours(v)
    } {
      g.addEdge(v,n)
    }
    g
  }
}

object Topology {
  type Aux[T] = Topology {type N = T}
}



