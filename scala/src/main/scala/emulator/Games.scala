package icfp
package emulator

trait Games {
  self: Commands with Games with Items with Points with States with Worlds =>

  def mkGame(world: World): InProgress = InProgress(world, 0, 0, 0)

  def stepGame(g: State, c: Command): State =
    g match {
      case g: InProgress =>
        c match {
          case Abort =>
            Aborted(g.w, g.steps + 1, g.collectedLambdas) // steps + 1 or steps?
          case _ =>
            var w = g.w

            val nextR = w.robot + c.dir
            val afterR = nextR + c.dir
            c match {
              case Up | Down | Left | Right if w(nextR).isPassable =>
                w = w.update(w.robot, Empty)
                w = w.update(nextR, Robot)
              case Left | Right if w(nextR) == Rock && w(afterR) == Empty =>
                w = w.update(w.robot, Empty)
                w = w.update(nextR, Robot)
                w = w.update(afterR, Rock)
              case _ => // do nothing
            }

            val stepsUnderwater1 = if (g.w.isUnderwater) g.stepsUnderwater + 1 else 0
            if (stepsUnderwater1 >= g.w.waterproof) w = w.update(w.robot, Empty)

            w = w.evolve

            val nextLambdas = g.collectedLambdas + g.w.remainingLambdas - w.remainingLambdas
            g.w(nextR) match {
              case _ if w.robot == Invalid =>
                Lost(w, g.steps + 1, nextLambdas)
              case Open =>
                Won(w, g.steps + 1, nextLambdas)
              case _ =>
                InProgress(w, g.steps + 1, nextLambdas, stepsUnderwater1)
            }
        }
      case _ =>
        g
    }

  def playGame(game: State, commands: Commands): State =
    commands.foldLeft(game)((s, c) =>
      s match {
        case g: InProgress =>
          // println("Step: %s".format(c))
          // println(g.w)
          // println()
          g.step(c)
        case _ =>
          s
      }
    )
}
