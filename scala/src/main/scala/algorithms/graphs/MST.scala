package icfp.algorithms.graphs

import icfp.algorithms.core._

// source: http://code.google.com/p/scalgorithm

trait MSTAlgo[A]{
  def generateMST(graph:Graph[A]): Tree[Vertex[A]]
}