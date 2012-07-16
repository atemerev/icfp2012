package icfp.algorithms.core

// source: http://code.google.com/p/scalgorithm

object Vertex{

  def apply[A,B](data:A) : Vertex[A,B] = {
    return new Vertex[A,B](data)
  }
}

class Vertex[A,B](val data:A){
  val edges:scala.collection.mutable.Set[Edge[A,B]] = scala.collection.mutable.HashSet[Edge[A,B]]()
  val _inEdges:scala.collection.mutable.Set[Edge[A,B]] = scala.collection.mutable.HashSet[Edge[A,B]]()
  var tag:Any = None

  def addEdge(edge:Edge[A,B]) : Unit = {
    edges.add(edge)
  }

  def getEdges() : Set[Edge[A,B]] = {
    edges.toSet
  }

  //TODO add test cases for INEDGES 
  def inEdges = _inEdges
  def addInboundEdge(edge:Edge[A,B]) { _inEdges.add(edge) }

  def adjacent() : Set[Edge[A,B]] = {
    getEdges().filter(e => e.v2 != this)
  }

  override def toString() : String = {
    var str = data.toString.concat(" -> ");
    /*    var str = data.toString.concat(" "+tag+" -> ");
    */
    edges.foreach(e => {(e.v1,e.v2) match{
      case (a,b) => if(a == this) str = str.concat(b.data.toString+' ') else str = str.concat(a.data.toString+' ')
    }
      str = str.concat("("+e.weight+") ")
    }
    )
    str
  }

  override def equals(other: Any) =
    other.isInstanceOf[Vertex[A,B]] && data == other.asInstanceOf[Vertex[A, B]].data
}

object Edge{
  def apply[A,B](value: B, weight:Double, v1:Vertex[A,B], v2:Vertex[A,B],directed:Boolean) : Edge[A,B] = {
    return new Edge[A,B](value, weight,v1,v2,directed,false)
  }
}

class Edge[A, B](val value: B, var weight:Double, val v1:Vertex[A,B],val v2:Vertex[A,B],val directed:Boolean,var highlighted:Boolean){

  var tag:Any = None

  directed match{

    case true => v1.addEdge(this);v2.addInboundEdge(this) // add only to the parent node in case of a directed graph  //TODO add test case for inbound edge 
    case false => v1.addEdge(this);v2.addEdge(this)
  }

  val vertices = List(v1,v2)

  def getOther(v:Vertex[A,B]) : Vertex[A,B] = {
    v match{
      case vertex if(vertex == v1) => v2
      case vertex if(vertex == v2) => v1
      case _ => null
    }
  }

  override def equals(other:Any) = other match{
    case that:Edge[A,B] => that.weight == this.weight && that.v1 == this.v1 && that.v2 == this.v2 && this.directed == that.directed
    case _ => false
  }

  override def toString = {
    "" + value// + "(" + weight + "): " + v1.data.toString + (if (directed) "->\n" else "---\n") + v2.data
  }
}

object Graph{
  def apply[A,B](vertices:List[Vertex[A,B]],directed:Boolean) : Graph[A,B] = {
    return new Graph[A,B](vertices,directed)
  }
}

class Graph[A,B](val v:List[Vertex[A,B]],val directed:Boolean) {


  val valVertexMap = scala.collection.mutable.Map[A,Vertex[A,B]]() //TODO .. this should not be exposed to the outer world

  val vertices: scala.collection.mutable.Set[Vertex[A,B]] = new scala.collection.mutable.HashSet[Vertex[A,B]]()
  ++(v)

  def _vertices = vertices

  val edges: scala.collection.mutable.Set[Edge[A,B]] = scala.collection.mutable.HashSet[Edge[A,B]]()
  //  def edges:List[Edge[A,B]] = edgeMap.values.toList

  var edgeMap = Map[Tuple2[Vertex[A,B],Vertex[A,B]],Edge[A,B]]();

  def ++(v:Vertex[A,B]) = {
    addVertex(v)
  }
  def ++(list:List[Vertex[A,B]]) = { list foreach addVertex}

  def addVertex(v:Vertex[A,B]) = {
    if (valVertexMap contains v.data) {
//      println("Warning! We already have " + v.data + " in the graph") // no problem, we grow the graph... or maybe we need a special kind of graph, the one that grows
      valVertexMap(v.data)
    }
//    assume(valVertexMap.get(v.data) == None)
    else {
      valVertexMap(v.data) = v
      vertices add v
      v
    }

  }

//  def ++(a:A) = {
//    addVertex(new Vertex[A,B](a))
//  }

  def addEdge(value: B, from:Vertex[A,B],to:Vertex[A,B],weight:Double):Edge[A,B] = {
    assume(vertices.exists(_ ==from) && vertices.exists(_ == to))
    addEdge(new Edge[A,B](value, weight,from,to,directed,false))
  }

  def addEdge(edge: Edge[A,B]) = {
    edges.add(edge)
    edge
  }

  def addEdge(tag:B, from:Vertex[A,B],to:Vertex[A,B]):Edge[A,B] = {
    return addEdge(tag, from,to,0)
  }

  def ::(tag_from_to:(B, Vertex[A,B],Vertex[A,B])):Edge[A,B] = {
    addEdge(tag_from_to._1,tag_from_to._2,tag_from_to._3)
  }

  def ::(tag_from_to_wt:(B, Vertex[A,B],Vertex[A,B],Double)):Edge[A,B] = {
    addEdge(tag_from_to_wt._1,tag_from_to_wt._2,tag_from_to_wt._3,tag_from_to_wt._4)
  }

  def vertex(data:A) : Option[Vertex[A,B]] = {
    valVertexMap.get(data)
  }

  def getEdge(from:Vertex[A,B],to:Vertex[A,B]): Option[Edge[A,B]] = {

    edges.find(e => ((e.v1 == from && e.v2 == to) ||(!directed && e.v1 == to && e.v2 == from) ))

    /*    val edge = edgeMap.get((from,to))
        
        if(edge == None & directed) return None
        else return edgeMap.get((to,from))
    */
  }
//  //MOST OF THE CODE HERE IS INEFFICIENT... but works for now and that's what matters to me :)
//  def removeEdge(from:Vertex[A,B],to:Vertex[A,B]){
//    val edgeOpt = edges.find(e => e.v1 == from && e.v2 == to)
//    if(edgeOpt == None) return;
//    val edge = edgeOpt.get
//    edge.vertices.foreach(_.removeEdge(edge))
//    edges = edges..remove(_ == edge)
//  }

//  def removeEdge(fromTo:Tuple2[Vertex[A,B],Vertex[A,B]]){
//    removeEdge(fromTo._1,fromTo._2)
//  }



//  def transpose(withTag:Boolean) : Graph[A,B] = {
//    assume(directed,"Graph is not directed")
//    val xVertices:List[Vertex[A,B]] = vertices.map( v => {
//      v.edges = List()
//      if(!withTag) v.tag = None
//      v
//    });
//    val xGraph = Graph[A,B](xVertices,directed)
//
//    xGraph.edges = edges.map[Edge[A,B], List[Edge[A,B]]](e => {Edge[A,B](e.weight,e.v2,e.v1,e.directed)})
//    xGraph
//  }

  def clearTags = vertices.foreach(_.tag = null)

  override def toString():String = {
    var str:String = "Graph ("+vertices.size+","+edges.size+") = \n"
    vertices.foreach(v => str = str.concat("\t"+v.toString).concat("\n"))
    str
  }
}
