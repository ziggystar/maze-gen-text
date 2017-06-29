package laby

/** Integer coordinate. */
case class Pos(x: Int, y: Int){
  def up: Pos = Pos(x,y+1)
  def down: Pos = Pos(x,y-1)
  def left: Pos = Pos(x-1,y)
  def right: Pos = Pos(x+1,y)
  def neighbours = Set(up, down, left, right)

  def +(o: Pos): Pos = Pos(x + o.x, y + o.y)
  def toPoint = Point(x,y)
}
