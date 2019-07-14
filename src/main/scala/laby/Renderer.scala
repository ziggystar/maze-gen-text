package laby

trait Renderer[T <: Topology, Out] {
  type N = T#N
  def render(topology: T, labels: N => Char, edges: Set[BiSet[N]]): Out
}
