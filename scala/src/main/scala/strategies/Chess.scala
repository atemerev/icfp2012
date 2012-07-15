package icfp
package strategies

trait Chess {

  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds with Pathfinder =>

  val MAX_LEVEL = 4

  var bestAborts = List[State]()
  var lambdaMaps = Map[Point, DistanceMap]()
  var liftMap: DistanceMap = null

  def score(state: State): Int = {
    // limited path to nearest lambda
    // lambdas collected
    // lift is open? then distance to lift
    // robot is no more :(
    if (state.w.robot == Invalid) Int.MinValue else {
      val lambdas = state.collectedLambdas
      val open = if (state.w.liftIsOpen) 1 else 0
//      val distToLambda = if (state.w.liftIsOpen) 0 else state.w.remainingLambdaPositions.map(p => p.distanceTo(state.w.robot)).min
      val distToLambda = if (state.w.liftIsOpen) 0 else {
        state.w.remainingLambdaPositions map (p => lambdaMaps(p)(state.w.robot.x)(state.w.robot.y)) min
      }
      val distToLift = if (state.w.liftIsOpen) liftMap(state.w.robot.x)(state.w.robot.y) else 1000
      lambdas * 100 + open * 1000 + (2000 / distToLift) * open - distToLambda * 10
    }
  }

  val cache = collection.mutable.Map[(InProgress, Command), State]()

  def step(ip: InProgress, c: Command) = cache.getOrElseUpdate((ip, c), ip.step(c))

  case class Node(parent: Node, command: Command, state: State, level: Int, aborted: Boolean) {
    lazy val children: List[Node] = {
      val commands = List(Up, Down, Left, Right, Abort, Wait)
      if (level > MAX_LEVEL) Nil else commands.flatMap(c => state match {
        case ip: InProgress if step(ip, c).w.robot != state.w.robot => Some(Node(this, c, step(ip, c), level + 1, c == Abort))
        case _ => None
      })
    }
    lazy val leaves: List[Node] = if (children.nonEmpty) children.flatMap(_.leaves) else List(this)

    lazy val commands: Commands = if (parent == null) Nil else parent.commands.toList :+ command
  }

  def mkTree(state: State) = Node(null, null, state, 1, false)

  def chess(game: State, trace: Boolean): Commands = {
    var g = game
    while (!g.terminal && g.commands.size <= g.w.w * g.w.h) {
      if (trace) println(g.w)
      lambdaMaps = g.w.remainingLambdaPositions map (p => p -> mkDistMap(g.w, p)) toMap;
      liftMap = mkDistMap(g.w, g.w.lift)
      val leaves = mkTree(g).leaves
      if (trace) {
//        leaves.sortBy(l => -score(l.state)).foreach(l => println("%s: %s".format(l.commands.mkString, score(l.state))))
      }
      val (aborts, games) = leaves partition (_.aborted)
      val bestGame = games.maxBy(l => score(l.state))
      val bestAbort = aborts.maxBy(l => score(l.state))
      bestAborts :+= bestAbort.state
      g = bestGame.state
    }
    val bestAbort = (bestAborts :+ game) maxBy (s => score(s))
    val bestResult = List(bestAbort, g) maxBy (s => score(s))
    val commands = bestResult.commands
    if (trace) {
      println(bestResult.w)
      println(commands mkString)
      println(bestResult.score)
    }
    commands
  }
}
