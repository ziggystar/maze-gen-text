package laby

import java.io.{File, FileOutputStream}

import scopt.{OptionParser, Read}

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Random

object Main {

  case class Path(start: Pos, end: Pos, text: String){
    def length: Int = text.length
  }

  case class Conf(topo: Either[Rect, FreeGrid] = Left(Rect()), paths: Seq[Path] = Seq(), maxTries: Int = 50, fillRandom: Boolean = false, standalone: Boolean = false, seed: Long = Random.nextLong()) {
    def run(): Unit = {
      val preamble =
        "\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[scale=0.3,Border/.style={}, StartNode/.style={}]"
      val postamble =
        "\\end{tikzpicture}\n\\end{document}"

      println(s"Used random generator seed: $seed")
      val rand: Random = new Random(seed)

      val graph: UGraph[Pos] = topo.fold(_.graph, _.graph)
      val pathLayout: Map[Path, Seq[Pos]] = paths.foldLeft(Map[Path,Seq[Pos]]()){case (visited, path) =>
          val sub = graph.subgraph(graph.vertexSet().asScala.toSet -- visited.values.flatten.toSet)
          val p = pathOfLength(sub, path.start, path.end, path.length, maxTries, rand).getOrElse(sys.error(s"could not find path for text '${path.text}"))
          visited + (path -> p)
      }

      val pathEdges: Set[BiSet[Pos]] = pathLayout.values.flatMap { p => p.sliding(2).map(e => BiSet(e(0), e(1))) }(collection.breakOut)

      val pathChars: Map[Pos, Char] = pathLayout.map{case (p,layout) => layout.zip(p.text.padTo(layout.size,' ')).toMap}.foldLeft(Map[Pos,Char]())(_ ++ _)
      val woPaths = graph.subgraph(graph.nodes -- pathLayout.values.flatten.toSet)

      val filled: Set[BiSet[Pos]] = spanningTrees(woPaths)

      val posToChar: Map[Pos, Char] =
        if(fillRandom) pathChars.withDefault(_ => ('A' + Random.nextInt(26)).toChar)
        else pathChars.withDefaultValue(' ')

      val r: String = Grid.TikzRenderer.render(
        topo.fold(identity,identity),
        posToChar,
        filled ++ pathEdges
      )
      val all: String = if(standalone) Seq(preamble, r, postamble).mkString("\n") else r

      val fo: FileOutputStream = new FileOutputStream("out.tex")
      fo.write(all.getBytes)
      fo.close()

      if(standalone) {
        import sys.process._
        "latexmk -pdf out.tex" !
      }
    }
  }

  implicit val readPath: Read[Path] = new Read[Path]{
    override def arity: Int = 1
    override def reads: String => Path = { input: String =>
      val pattern = "\\((.*)\\)(.*)\\((.*)\\)".r
      val result = pattern.findFirstMatchIn(input).get
      def parseTuple(t: String): Pos = {
        val split = t.split(",")
        Pos(split(0).toInt,split(1).toInt)
      }
      Path(parseTuple(result.group(1)),parseTuple(result.group(3)), result.group(2))
    }
  }

  val parser: OptionParser[Conf] = new OptionParser[Conf]("maze-gen") {
    opt[Int]("width")
      .text("width of rectangular labyrinth")
      .action{case (w,c) => c.copy(topo = c.topo match {
      case Left(Rect(_, h)) => Left(Rect(w, h))
      case _ => Left(Rect(width = w))
    })}
    opt[Int]("height")
      .text("height of rectangular labyrinth")
      .action{case (h,c) => c.copy(topo = c.topo match {
      case Left(Rect(w, _)) => Left(Rect(w, h))
      case _ => Left(Rect(height = h))
    })}
    opt[File]("image")
      .text("use shape from this b/w image")
      .action{case (f, c) => c.copy(topo = Right(FreeGrid.fromImage(f).get))}

    opt[Path]("text")
      .text("fill this text into the labyrinth")
      .action{case (p,c) => c.copy(paths = c.paths :+ p)}
    opt[Unit]("fill")
      .text("fill the remaining labyrinth with random characters")
      .action{case (_,c) => c.copy(fillRandom = true)}
    opt[Unit]("standalone")
      .text("compile to PDF using latexmk")
      .action{case (_,c) => c.copy(standalone = true)}
    opt[Long]("seed")
      .text("random generator seed, use to get reproducible results")
      .action{case (l,c) => c.copy(seed = l)}
  }

  def main(args: Array[String]): Unit = {
    parser.parse(args, Conf()).foreach(_.run())
  }
}
