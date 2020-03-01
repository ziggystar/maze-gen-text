package laby

case class Rect(width: Int = 10, height: Int = 10) extends Grid {
  override val nodes: Set[Pos] =
    (for(x <- (0 until width).view; y <- 0 until height) yield Pos(x,y)).to(Set)
}
