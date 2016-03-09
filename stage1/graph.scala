package cbt

case class Graph[T](nodes: Set[T], _dependsOn: Map[T, Set[T]]) {

  val dependsOn = _dependsOn.withDefaultValue(Set())

  /** Nodes with no dependencies */
  lazy val sinks: Set[T] =
    nodes -- dependsOn.keys

  lazy val dependedOn = dependsOn.view.toSeq.flatMap { case (k,vs) =>
    vs.map(_ -> k)
  }.groupBy(_._1).mapValues(_.map(_._2).toSet)
   .withDefaultValue(Set())

}

object Graph {

  def breadthFirstSearch[T](g: Graph[T]): Stream[Set[T]] = {
    def rec(ns: Set[T], processed: Set[T]): Stream[Set[T]] = {
      if (ns.isEmpty) Stream.empty
      else {
        val allDependedOns = ns.flatMap(g.dependedOn)
        ns #:: rec(allDependedOns -- processed, processed ++ ns)
      }
    }

    rec(g.sinks, Set())
  }
}
