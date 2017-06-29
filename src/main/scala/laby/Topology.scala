package laby

import java.awt.Color
import java.io.{File, FileOutputStream}
import java.{io, util}
import javax.imageio.ImageIO

import laby.MazeGen.{RichGraph, UGraph}
import org.jgrapht.{Graphs, UndirectedGraph}
import org.jgrapht.alg.ConnectivityInspector
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.alg.spanning.KruskalMinimumSpanningTree
import org.jgrapht.graph.{DefaultEdge, SimpleGraph, Subgraph}
import scopt.{OptionParser, Read}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Random, Try}

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

object MazeGen {
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
    println(s"trying to find path of length: ${length}")
    val initialDistance: Double = {
      val d = new DijkstraShortestPath(_g)
      d.getPath(start,end).getLength.toDouble
    }
    def desiredDistance(pathLength: Int): Double = initialDistance - pathLength * (initialDistance/length) + 5
    def genOnePath() = {
      val g = _g.copy
      val r = Iterator.iterate(Option(List(start))){
        case Some(path@(h::t)) if h != end =>
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
      println(r.length)
      r
    }
    Iterator.continually(genOnePath()).take(maxTries).find(_.length >= length)
  }

  def spanningTrees[N](g: UGraph[N]): Set[BiSet[N]] = {
    val k = new KruskalMinimumSpanningTree(g)
    k.getSpanningTree.getEdges.asScala.toSet.map((e: DefaultEdge) => BiSet(g.getEdgeSource(e),g.getEdgeTarget(e)))
  }
}

trait Renderer[T <: Topology, Out] {
  type N = T#N
  def render(topology: T, labels: N => Char, edges: Set[BiSet[N]]): Out
}

trait Grid extends Topology {
  type N = Pos
  override def neighbours(n: N): Set[N] = n.neighbours filter nodes
}

case class Rect(width: Int = 10, height: Int = 10) extends Grid {
  override val nodes: Set[Pos] =
    (for(x <- 0 until width; y <- 0 until height) yield Pos(x,y))(collection.breakOut)
}

case class FreeGrid(nodes: Set[Pos]) extends Grid

object FreeGrid {
  def fromImage(file: File): Try[FreeGrid] = Try{
    val i = ImageIO.read(file)
    val cells = for{
      x <- 0 until i.getWidth
      y <- 0 until i.getHeight if i.getRGB(x,y) != 0
    } yield Pos(x,y)
    FreeGrid(cells.toSet)
  }
}

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

  def asciiRenderer: Renderer[Rect,String] = new Renderer[Rect,String]{
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

  /* TODO
   - draw border
   - mark beginning node
   */
  def tikzRenderer: Renderer[Grid,String] = new Renderer[Grid,String]{

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

object Main {

  case class Path(start: Pos, end: Pos, text: String){
    def length: Int = text.length
  }

  case class Conf(topo: Either[Rect, FreeGrid] = Left(Rect()), paths: Seq[Path] = Seq(), maxTries: Int = 50, fillRandom: Boolean = false, standalone: Boolean = false, seed: Option[Long] = None) {
    def run(): Unit = {
      val preamble =
        "\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[scale=0.3,Border/.style={}, StartNode/.style={}]"
      val postamble =
        "\\end{tikzpicture}\n\\end{document}"

      println(seed)
      val rand: Random = seed.map(s => new Random(s)).getOrElse(Random.self)

      val graph: UGraph[Pos] = topo.fold(_.graph, _.graph)
      val pathLayout: Map[Path, Seq[Pos]] = paths.foldLeft(Map[Path,Seq[Pos]]()){case (visited, path) =>
          val sub = graph.subgraph(graph.vertexSet().asScala.toSet -- visited.values.flatten.toSet)
          val p = MazeGen.pathOfLength(sub, path.start, path.end, path.length, maxTries, rand).getOrElse(sys.error(s"could not find path for text '${path.text}"))
          visited + (path -> p)
      }

      val pathEdges: Set[BiSet[Pos]] = pathLayout.values.flatMap { p => p.sliding(2).map(e => BiSet(e(0), e(1))) }(collection.breakOut)

      val pathChars: Map[Pos, Char] = pathLayout.map{case (p,layout) => layout.zip(p.text.padTo(layout.size,' ')).toMap}.foldLeft(Map[Pos,Char]())(_ ++ _)
      val woPaths = graph.subgraph(graph.nodes -- pathLayout.values.flatten.toSet)

      val filled: Set[BiSet[Pos]] = MazeGen.spanningTrees(woPaths)

      val posToChar =
        if(fillRandom) pathChars.withDefault(_ => ('A' + Random.nextInt(26)).toChar)
        else pathChars.withDefaultValue(' ')

      val r = Grid.tikzRenderer.render(
        topo.fold(identity,identity),
        posToChar,
        filled ++ pathEdges
      )
      val all = if(standalone) Seq(preamble, r, postamble).mkString("\n") else r

      val fo = new FileOutputStream("out.tex")
      fo.write(all.getBytes)
      fo.close()

      if(standalone) {
        import sys.process._
        "latexmk -pdf out.tex" !
      }
      //    val pol: Seq[(Int, Int)] = Topology.pathOfLength(grid,desiredLength, -2)
      //    println(s"desired: $desiredLength, found: ${pol.size}")

    }
  }

  implicit val readPath = new Read[Path]{
    override def arity: Int = 1
    override def reads: String => Path = { (input: String) =>
      val pattern = "\\((.*)\\)(.*)\\((.*)\\)".r
      val result = pattern.findFirstMatchIn(input).get
      def parseTuple(t: String): Pos = {
        val split = t.split(",")
        Pos(split(0).toInt,split(1).toInt)
      }
      Path(parseTuple(result.group(1)),parseTuple(result.group(3)), result.group(2))
    }
  }

  val parser = new OptionParser[Conf]("maze-gen") {
    opt[Int]("width").action{case (w,c) => c.copy(topo = c.topo match {
      case Left(Rect(_, h)) => Left(Rect(w, h))
      case _ => Left(Rect(width = w))
    })}
    opt[Int]("height").action{case (h,c) => c.copy(topo = c.topo match {
      case Left(Rect(w, _)) => Left(Rect(w, h))
      case _ => Left(Rect(height = h))
    })}
    opt[File]("image").action{case (f, c) => c.copy(topo = Right(FreeGrid.fromImage(f).get))}

    opt[Path]("text").action{case (p,c) => c.copy(paths = c.paths :+ p)}
    opt[Unit]("fill").action{case (_,c) => c.copy(fillRandom = true)}
    opt[Unit]("standalone").action{case (_,c) => c.copy(standalone = true)}
    opt[Long]("seed").action{case (l,c) => c.copy(seed = Some(l))}
  }

  def main(args: Array[String]): Unit = {
    parser.parse(args, Conf()).foreach(_.run())
  }
}


