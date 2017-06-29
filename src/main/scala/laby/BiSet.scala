package laby

case class BiSet[N](n1: N, n2: N){
  override def equals(obj: scala.Any): Boolean = obj match {
    case BiSet(o1, o2) => (n1 == o1 && n2 == o2) || (n1 == o2 && n2 == o1)
  }
  override def hashCode(): Int = Set(n1,n2).hashCode()
}
