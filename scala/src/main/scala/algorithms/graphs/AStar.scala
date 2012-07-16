package icfp.algorithms.graphs

import collection.mutable.{HashMap, HashSet}
import icfp.algorithms.core._
import icfp.Trace

// source: http://code.google.com/p/scalgorithm

class AStar[A,B] {

  type searchNodeClass = (Double,Double,List[(Vertex[A,B], Edge[A, B])]) //f,g,path
  implicit def searchNodeClassToOrdered(thisSearchNode:searchNodeClass) : Ordered[searchNodeClass] = new Ordered[searchNodeClass]{
    def compare(thatSearchNode:searchNodeClass):Int = {
      val d = thatSearchNode._1 - thisSearchNode._1
      if (d > 0) -1 else if (d < 0) 1 else 0
    }
  }

  def path(node: searchNodeClass) = node._3.reverse map (_._2) filter (null !=)

  def search(graph:Graph[A,B],s:Vertex[A,B],goal:(Vertex[A,B]) => Boolean,h:(Vertex[A,B]) => Double): List[Edge[A,B]] = {
    val node = searchTechicalities(graph, s, goal, h)
    val p = path(node)
    if (Trace.isEnabled) println("found: " + node + "(" + p + ")")
    p
  }

  def searchTechicalities(graph:Graph[A,B],s:Vertex[A,B],goal:(Vertex[A,B]) => Boolean,h:(Vertex[A,B]) => Double): searchNodeClass = {
    assume(graph.vertices.contains(s))
    s.tag = (0,Nil)
    val Q = new BinomialHeap[searchNodeClass]((-1,0, Nil))
    Q += (0,0,List((s, null)))

    val expanded = new HashSet[Vertex[A,B]]()
    val Qmap = new HashMap[Vertex[A,B],searchNodeClass]()

    while(!Q.empty){
//      println("\n==========\n" + Q + "\n-----------------\n")
      val N = Q.extractMin.get
      val p = N._3.head
      val u = p._1
      Qmap -= u
      if (Trace.isEnabled) println("trying\n" + u + "("+ path(N).mkString("") + "->" + N._1+ "/" + N._2 + "); have + " + Qmap.size)
      if(goal(u)){
        return  N
      }
      //no point expanding u if it has already been expanded on some other route ...
      if(!expanded.contains(u)){
        expanded += u
        val g = _g(graph,u,N._2)_
        val adj: Set[Edge[A, B]] = u.adjacent
        adj.foreach( e => if(!expanded.contains(e.v2)){
          val v = e.v2
          val gVal = g(v)
          val f = gVal + h(v)
          val Nv = (f,gVal,(v,e)::N._3)
          if(Qmap.contains(v)){// if there already exists a path to this node from some other route
            val present = Qmap(v)
            if(present._2 > gVal){ // and the cost of that path is greater than this one
//              if (Trace.isEnabled) println("Replacing " + present + "with" + Nv)
              // replace present with new one
              Qmap(v) = Nv
              // then remove that path from the Queue
              Q -= present
              // and add the new path to the Queue
              Q += Nv
            }
          }else{
//            if (Trace.isEnabled) println("" + Nv + ": NEVER SEEN BEFORE? we have " + Qmap.size + " nodes known")
            Qmap += v -> Nv
            Q += Nv
          }
        })

      }
    }

    assume(false,"No Solution")
    null
  }

  def _g(graph:Graph[A,B],u:Vertex[A,B],g:Double)(v:Vertex[A,B]) : Double = {
    assume(graph.getEdge(u,v) != None)
    return g + graph.getEdge(u,v).get.weight
  }

}
