package laby

case class Point(x: Double, y: Double) {
  def +(o: Point): Point = Point(x + o.x, y + o.y)
  def *(s: Double): Point = Point(x * s, y * s)
  def *(o: Point): Double = x * o.x + y * o.y
  def -(o: Point): Point = this + (o * -1)
  def rot: Point = Point(y, -x)
  def flipy: Point = Point(x,-y)
  def projx: Point = Point(x,0)
  def projy: Point = Point(0,y)

  override def toString: String = s"($x,$y)"
}
