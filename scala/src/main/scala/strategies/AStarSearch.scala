package icfp
package strategies

import algorithms.graphs.AStar
import emulator._
import algorithms.core.{Edge, Graph, Vertex}
import collection.immutable.{Iterable, Map}
import strategies.AStarSearch.EState

trait AStarSearch {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  case class EState(state: State) {
    private def world = state.w
    override def hashCode: Int = state.w.hashCode
    override def equals(x: Any) = x.isInstanceOf[EState] &&
    state.w == x.asInstanceOf[EState].world
    def step(c: Command) = new EState(stepGame(state, c))
    def hasAllLambdas = state.haveAllLambdas
    var isDeadEnd = false
    def isBad = isDeadEnd || {
      val r = state.status == "lost" || !state.mayGetToLift
      r
    }
    def eval: Double = {
      val sumDistances = world.distanceToLambdas(world.lift) + world.distanceToNearestLambda(world.robot)
      if (isBad) SOME_STUPID_BIG_NUMBER else
      if (sumDistances > 0) sumDistances * 100 + 1000 else
                            world.fromRobotToLift
    }
    def theEnd = state.status == "won"
    override def toString = world.toString
  }

  val SOME_STUPID_BIG_NUMBER:Double = 1000000.
  val knownCommands = List(Up, Right, Down, Left, Wait)
  val weight = Map[Command, Double](Left -> 1, Right -> 1, Up -> 1, Down -> 1, Wait -> 0.5)
  val graph: Graph[EState, Command] = Graph(Nil, true)

  def vertex(state: State): Vertex[EState, Command] = vertex(EState(state))

  def vertex(estate: EState): Vertex[EState, Command] = {
    val result: Vertex[EState, Command] = new Vertex[EState, Command](estate) {
      private var knowsNeighbors = false;

      private def newEdge(command: Command, state: EState) =  new Edge(command, weight(command), this, vertex(state), true, false)

      private def createNeighbors {
        val neighbors: Map[EState, Command] = knownCommands.map( (c: Command) => (estate.step(c), c) ).toMap.filterKeys(estate!=)
        val goodNeighbors: Map[EState, Command] = neighbors.filterKeys(newEstate => !newEstate.isBad)
        val newEdges = goodNeighbors map { case (estate, command) => newEdge(command, estate) }
        if (newEdges.isEmpty) {
          estate.isDeadEnd = true
        }
        val sortedEdges = newEdges.toList.sort((e1, e2) => e1.value.toString.compare(e2.value.toString) == 1)
        sortedEdges foreach ((e: Edge[EState, Command]) => graph.addEdge(e))
      }

      override def adjacent() : Seq[Edge[EState, Command]] = {
        if (!knowsNeighbors) {
          createNeighbors
          knowsNeighbors = true
        }
        getEdges
      }
    }
    graph.addVertex(result)
  }

  def eval(node: Vertex[EState, Command]): Double = node.data.eval

  def search(state: State, timeout: Int): Commands = {
    val estate = EState(state)
    if (Trace.isEnabled) println("Running " + state)
    findPath(estate, (e => e.theEnd))._1
  }

  def getToLift(start: EState): (Commands, EState) = findPath(start, (e => e.theEnd))

  def collectLambdas(start: EState): (Commands, EState) = findPath(start, (e => e.hasAllLambdas))

  def findPath(start: EState, isGoal: EState => Boolean): (Commands, EState) = {
    val astar = new AStar[EState,Command]

    def isLastNode(node: Vertex[EState, Command]): Boolean = {
      isGoal(node.data)
    }

    try {
      val result = astar.search(graph, vertex(start), isLastNode, eval)
      val chain = result filter (null!=) map (_.value)
      (chain, result.last.v2.data)
    } catch {
      case x =>{
        if (Trace.isEnabled) {
          println("oops, " + x)
          x.printStackTrace()
        }
        (self.mkCommands("A"), start)
      } // actually, have to get max
    }
  }
}

object AStarSearch extends AStarSearch with DumbEmulator
