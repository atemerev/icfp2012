package icfp.algorithms.graphs

import icfp.algorithms.core.{Graph, Tree, Vertex}


// source: http://code.google.com/p/scalgorithm

trait ShortestPathAlgo[A]{
  def getShortestPath(graph:Graph[A], s:Vertex[A]): Tree[Vertex[A]]
}
