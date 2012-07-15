package icfp
package strategies

trait Chess {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds with Pathfinder =>

  def chess(game: State, timeout: Int, trace: Boolean): Commands = {
    val MAX_LEVEL = 4

    var bestAborts = Map[State, Int]()
    var lambdaMaps = Map[Point, DistanceMap]()
    var liftMap: DistanceMap = null

    def score(state: State): Int = {
      // limited path to nearest lambda
      // lambdas collected
      // lift is open? then distance to lift
      // robot is no more :(
      if (state.w.robot == Invalid) Int.MinValue else {
        val lambdas = state.w.collectedLambdas
        val open = if (state.w.liftIsOpen) 1 else 0
  //      val distToLambda = if (state.w.liftIsOpen) 0 else state.w.remainingLambdaPositions.map(p => p.distanceTo(state.w.robot)).min
        val distToLambda = if (state.w.liftIsOpen) 0 else {
          state.w.remainingLambdaPositions map (p => lambdaMaps(p)(state.w.robot.x)(state.w.robot.y)) min
        }
        val distToLift = if (state.w.liftIsOpen) liftMap(state.w.robot.x)(state.w.robot.y) else 1000
        lambdas * 300 + open * 1000 + (2000 / distToLift) * open - distToLambda * 10
      }
    }

    val cache = collection.mutable.Map[(InProgress, Command), State]()

    def step(ip: InProgress, c: Command) = cache.getOrElseUpdate((ip, c), ip.step(c))

    case class Node(parent: Node, command: Command, state: State, level: Int, aborted: Boolean) {
      lazy val children: List[Node] = {
        val commands = List(Up, Down, Left, Right, Abort, Wait)
        if (level > MAX_LEVEL) Nil else commands.flatMap(c => state match {
          case ip: InProgress if step(ip, c).w.robot != state.w.robot || step(ip, c).isInstanceOf[Aborted] => Some(Node(this, c, step(ip, c), level + 1, c == Abort))
          case _ => None
        })
      }
      lazy val leaves: List[Node] = if (children.nonEmpty) children.flatMap(_.leaves) else List(this)

      lazy val commands: Commands = if (parent == null) Nil else parent.commands.toList :+ command
    }

    def mkTree(state: State) = Node(null, null, state, 1, false)

    val start = System.currentTimeMillis()
    def isTimeout = (System.currentTimeMillis() - start) / 1000 > timeout
    var g = game
    var progress = List[Int](g.commands.size)
    var i = 0
    var reachable = true
    def isStuck = if (i > 10) {
      val delta = progress(i) - progress(i - 10)
      // println(progress)
      delta < 15
    } else false
    while (reachable && !isTimeout && !isStuck && !g.terminal && g.commands.size <= g.w.w * g.w.h) {
      if (trace) println(g.w)
      lambdaMaps = g.w.remainingLambdaPositions map (p => p -> mkDistMap(g.w, p)) toMap;
      reachable = g.w.remainingLambdas == 0 || (lambdaMaps.map(_._2(g.w.robot.x)(g.w.robot.y)).min < 1000)
      liftMap = mkDistMap(g.w, g.w.lift)
      val leaves = mkTree(g).leaves
      if (trace) {
        leaves.sortBy(l => -score(l.state)).foreach(l => println("%s: %s".format(l.commands.mkString, score(l.state))))
      }
      val (aborts, games) = leaves partition (_.aborted)
      val bestGame = games.maxBy(l => score(l.state))
      val bestAbort = aborts.maxBy(l => score(l.state))
      bestAborts += (bestAbort.state -> score(bestAbort.state))
      g = bestGame.state
 //     val nearest = lambdaMaps(Point(3, 13))
 //     println(nearest.transpose.map(x => x.map(i => if (i > 1000) "##" else "%2d".format(i)).mkString(" ")).reverse.mkString("\n"))
      //      for (lm <- lambdaMaps) {
//        println(lm._2.transpose.map(x => x.map(i => if (i > 1000) "#" else "%1d".format(i)).mkString("")).reverse.mkString("\n"))
//        println()
//      }
      i += 1
      progress :+= g.commands.size
    }
//    println(bestAborts.map({ case (s,i) => (s.commands.mkString, i)}).toList.sortBy(-_._2))
    val bestAbort = ((bestAborts + (game -> -1)) maxBy { case (s, i) => s.score })._1
    val bestResult = List(bestAbort, g) maxBy (_.score)
    val commands = bestResult.commands
    if (trace) {
      println(bestResult.w)
      println(commands mkString)
      println(bestResult.score)
    }
    commands
  }
}
