package icfp.algorithms.graphs

import collection.mutable.{HashMap, HashSet}
import icfp.algorithms.core.{Edge, FibonacciHeap, Graph, Vertex}


// source: http://code.google.com/p/scalgorithm

class AStar[A,B](trace: Boolean) {

  type searchNodeClass = (Double,Double,List[(Vertex[A,B], Edge[A, B])]) //f,g,path
  implicit def searchNodeClassToOrdered(thisSearchNode:searchNodeClass) : Ordered[searchNodeClass] = new Ordered[searchNodeClass]{
    def compare(thatSearchNode:searchNodeClass):Int = {
      //println("comparing : "+thisVertex.data+" - "+thatVertex.data)
      thatSearchNode._1 - thisSearchNode._1 match {
        case d if(d> 0) => -1
        case d if(d< 0) => 1
        case _ => 0
      }
    }
  }


  def search(graph:Graph[A,B],s:Vertex[A,B],goal:(Vertex[A,B]) => Boolean,h:(Vertex[A,B]) => Double): List[Edge[A,B]] = {
    val N = searchTechicalities(graph, s, goal, h)
    println(N)
    N._3.reverse map (_._2)
  }
  
  def searchTechicalities(graph:Graph[A,B],s:Vertex[A,B],goal:(Vertex[A,B]) => Boolean,h:(Vertex[A,B]) => Double): searchNodeClass = {
    assume(graph.vertices.contains(s))
    s.tag = (0,Nil)
    val Q = new FibonacciHeap[searchNodeClass]((-1,0,Nil))
    Q += (0,0,List((s, null)))

    val expanded = new HashSet[Vertex[A,B]]()
    val Qmap = new HashMap[Vertex[A,B],searchNodeClass]()

    while(!Q.empty){
      val N = Q.extractMin.get
      val p = N._3.head
      val u = p._1
      Qmap -= u
      println("trying" + p)
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
            if(Qmap(v)._2 > gVal){ // and the cost of that path is greater than this one
              // then remove that path from the Queue
              Q -= Qmap(v)
              Qmap -= v
              // and add the new path to the Queue
              Q += Nv
              Qmap += v -> Nv
            }
          }else{
            println("" + Nv + ": NEVER SEEN BEFORE?")
            Q += Nv
            Qmap += v -> Nv
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
