package laby

import java.io.File

import javax.imageio.ImageIO

import scala.util.Try

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