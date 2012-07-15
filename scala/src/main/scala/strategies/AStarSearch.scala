package icfp
package strategies

import algorithms.graphs.AStar
import emulator._
import algorithms.core.{Edge, Graph, Vertex}
import collection.immutable.{Iterable, Map}

trait AStarSearch {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>
  val SOME_STUPID_BIG_NUMBER:Double = 40876.
  val knownCommands = Set(Left, Right, Up, Down, Wait)
  val weight = Map[Command, Double](Left -> 1, Right -> 1, Up -> 1, Down -> 1, Wait -> 0.5)
  val graph: Graph[State, Command] = Graph(Nil, true)

  def vertex(state: State): Vertex[State, Command] = {

    val result: Vertex[State, Command] = new Vertex[State, Command](state) {
      private var knowsNeighbors = false;

      private def newEdge(command: Command, state: State) =  new Edge(command, weight(command), this, vertex(state), true, false)

      private def createNeighbors {
        val neighbors: Map[State, Command] = knownCommands.map( (c: Command) => (stepGame(state, c), c) ).toMap.filterKeys(state !=)
        val newEdges: Iterable[Edge[State, Command]] = neighbors map { case (state, command) => newEdge(command, state) }
        newEdges foreach ((e: Edge[State, Command]) => graph.addEdge(e))
      }

      override def adjacent() : Set[Edge[State, Command]] = {
        if (!knowsNeighbors) {
          createNeighbors
          knowsNeighbors = true
        }
        super.adjacent()
      }
    }
    graph.addVertex(result)
  }

  def hasAllLambdas(state: State): Boolean = {
    state.haveAllLambdas
  }

  def eval(node: Vertex[State, Command]): Double = if (node.data.mayGetToLift) node.data.w.remainingLambdas else SOME_STUPID_BIG_NUMBER * 2

  def search(state: State, timeout: Int, trace: Boolean): Commands = {
    if (trace) { println(state.w.lift) }
    if (trace) println("Running " + state)
    val withLambdas = collectLambdas(state, trace)
    val atLift = getToLift(withLambdas._2)
    withLambdas._1 ++ atLift._1
  }

  val theEnd = (state: State) => {
    if (Trace.isEnabled && (state.w.robot.distanceTo(state.w.lift) < 3)) println("Is " + state + " final?!?!")
    state.status == "won"
  }

  def getToLift(start: State, trace: Boolean = false): (Commands, State) = findPath(start, theEnd, trace)

  def collectLambdas(start: State, trace: Boolean = false): (Commands, State) = findPath(start, hasAllLambdas, trace)

  def findPath(start: State, isGoal: State => Boolean, trace: Boolean = false): (Commands, State) = {
    val astar = new AStar[State,Command](trace)

    def isLastNode(node: Vertex[State, Command]): Boolean = {
      isGoal(node.data)
    }

    try {
      val result = astar.search(graph, vertex(start), isLastNode, eval)
      if (trace) println(result)
      val chain = result filter (null!=) map (_.value)
      (chain, result.last.v2.data)
    } catch {
      case x =>{
        if (trace) {
          println("oops, " + x)
          x.printStackTrace()
        }
        (self.mkCommands("A"), start)
      } // actually, have to get max
    }
  }
}

object AStarSearch extends AStarSearch with DumbEmulator
