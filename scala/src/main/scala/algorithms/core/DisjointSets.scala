package icfp.algorithms.core

import scala.collection.mutable.{HashSet,HashMap}

// source: http://code.google.com/p/scalgorithm

trait DisjointSet[A] {

  def += (x:A) = makeSet(x)
  def apply(x:A):A = findSet(x)

  def makeSet(x:A)
  def findSet(x:A):A
  def union(x:A,y:A)

}

class DisjointSetForest[A] extends DisjointSet[A] {

  val sets = new HashSet[Tree[A]]();
  val map = new HashMap[A,Node[A]]();


  def makeSet(x:A) = {
    val set = Tree(x)
    map += x -> set.root
    sets += set

  }

  def findSet(x:A):A = {
    assume(map(x) != null)

    if(map(x).parent == None)
      return x
    else{
      map(x).parent = Some(map(findSet(map(x).parent.get.data)))
      map(x).parent.get.data
    }
  }

  def union(x:A,y:A) = {
    val xRoot = map(findSet(x))
    val yRoot = map(findSet(y))

    if(xRoot.degree > yRoot.degree){
      yRoot.parent = Some(xRoot)
    }else if(xRoot.degree < yRoot.degree){
      xRoot.parent = Some(yRoot)
    }else if(xRoot != yRoot){
      yRoot.parent = Some(xRoot)
      xRoot.degree = xRoot.degree + 1
    }
  }


}