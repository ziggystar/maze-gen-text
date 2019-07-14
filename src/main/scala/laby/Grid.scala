package laby

object Grid {
  case class NewPos(x: Int, y: Int)

  /** Calculate the coordinate on the output grid for an edge on the labyrinth grid. */
  def edgeToCoord(bs: BiSet[Pos]): NewPos = {
    val BiSet(from,to) = bs
    require(Set(math.abs(from.x - to.x), math.abs(from.y - to.y)) == Set(0,1))
    NewPos(from.x + to.x + 1, from.y + to.y + 1)
  }
  /** Calculate the position on the output grid for a node on the labyrinth grid. */
  def slotToCoord(n: Pos): NewPos = NewPos(n.x * 2 + 1, n.y * 2 + 1)

  object ASCIIRenderer extends Renderer[Rect,String]{
    override def render(topology: Rect, labels: Pos => Char, edges: Set[BiSet[Pos]]): String = {

      val result: Map[NewPos,Char] =
        (topology.nodes.map(n => slotToCoord(n) -> labels(n)).toMap ++
          edges.map(edgeToCoord).map(_ -> ' ')).withDefaultValue('#')
      (0 until (topology.height * 2 + 1)).map(row =>
        (0 until (topology.width * 2 + 1)).map(col =>
          result(NewPos(col,row))
        ).mkString
      ).mkString("\n")
    }
  }

  object TikzRenderer extends Renderer[Grid,String]{
    override def render(topology: Grid, labels: Pos => Char, edges: Set[BiSet[Pos]]): String = {
      def node(n: Pos, char: Char, style: Option[String] = None): String = s"\\node${style.map{s => s"[$s]"}.getOrElse("")} at ${n.toPoint.flipy} {$char};"
      def closedEdge(e: BiSet[Pos]): String = {
        val p1 = e.n1.toPoint
        val p2 = e.n2.toPoint
        val center = (p1 + p2) * 0.5
        val perp = (center - p1).rot
        val left = center +  perp
        val right = center - perp
        s"${left.flipy} -- ${right.flipy}"
      }
      val nodeString = topology.nodes.toSeq.filter(n => labels(n) != ' ').map(n => node(n, labels(n), Option("StartNode").filter(_ => false))).mkString("\n")
      val borders: Set[BiSet[Pos]] = for{
        n <- topology.nodes
        out <- n.neighbours if !topology.nodes.contains(out)
      } yield BiSet(n,out)
      val edgeString = "\\draw " + (topology.allEdges -- edges ++ borders).toSeq.map(closedEdge).grouped(5).map(es => "    " + es.mkString(" ")).mkString("\n") + ";"
      nodeString + "\n\n" + edgeString
    }
  }
}

trait Grid extends Topology {
  type N = Pos
  override def neighbours(n: N): Set[N] = n.neighbours filter nodes
}