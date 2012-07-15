package icfp
package emulator

trait Games {
  self: Commands with Games with Items with Points with States with Worlds =>

  def mkGame(world: World): InProgress = InProgress(world, Nil, 0)

  def stepGame(g: State, c: Command): State = maybeStep(g, c) getOrElse g

  def ifLegal(condition: Boolean): State => Option[State] = if (condition) (s => Some(s)) else (s => None)

  def maybeStep(g: State, c: Command): Option[State] =
    g match {
      case g: InProgress =>
        c match {
          case Abort =>
            Some(Aborted(g.w, g.commands :+ c)) // steps + 1 or steps?
          case _ =>
            var w = g.w
            // there=s a difference between steps and moves. W is a step but not a move. U when on top is neither, we should not be wasting resources
            var wasLegalStep = false
            var wasLegalMove = false
            val nextR = w.robot + c.dir
            val afterR = nextR + c.dir
            c match {
              case Up | Down | Left | Right if w(nextR).isPassable =>
                w = w.update(w.robot, Empty)
                w = w.update(nextR, Robot)
                wasLegalMove = true
              case Left | Right if w(nextR).isRock && w(afterR) == Empty =>
                val old = w(nextR)
                w = w.update(w.robot, Empty)
                w = w.update(nextR, Robot)
                w = w.update(afterR, old)
                wasLegalMove = true
              case Wait =>
                wasLegalStep = true
              case _ => // do nothing
            }
            if (g.w(nextR).isTrampoline) {
              w = w.update(nextR, Empty)
              // println(g.w.trampolines)
              val target = g.w.trampolines(g.w(nextR).asInstanceOf[Trampoline])
              w = w.update(target, Robot)
              val alsoRemove = g.w.trampolines.collect{ case (tramp1, target1) if target == target1 => tramp1 }
              val ptsOfAlsoRemove = alsoRemove flatMap (tramp => g.w.trampolinePositions filter (pt => g.w(pt) == tramp))
              ptsOfAlsoRemove foreach (ptTramp => w = w.update(ptTramp, Empty))
            }
            wasLegalStep |= wasLegalMove
            val stepsUnderwater1 = if (g.w.isUnderwater) g.stepsUnderwater + 1 else 0
            if (stepsUnderwater1 >= g.w.waterproof) w = w.update(w.robot, Empty)

            w = w.evolve

            if (!wasLegalStep) None else Some(
            if (w.isFinal)          Won(w, g.commands :+ c :+ w.finalCommand)  else
            if (w.robot == Invalid) Lost(w, g.commands :+ c) else
                                    InProgress(w, g.commands :+ c, stepsUnderwater1))
        }
      case _ =>
        None
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
