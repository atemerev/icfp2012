package icfp
package strategies

trait Chess {
  self: emulator.Commands with emulator.Games with emulator.Items with emulator.Points with emulator.States with emulator.Worlds =>

  def score(state: State): Int = {
    // limited path to nearest lambda
    // lambdas collected
    // lift is open? then distance to lift
    // robot is no more :(
    if (state.w.robot == Invalid) Int.MinValue else {
      val lambdas = state.collectedLambdas
      val open = if (state.w.liftIsOpen) 1 else 0
      val distToLambda = state.w.remainingLambdaPositions.map(p => p.distanceTo(state.w.robot)).min
      val distToLift = state.w.robot.distanceTo(state.w.lift)
      lambdas * 100 + open * 1000 + (2000 / distToLift) * open + distToLambda * 10
    }
  }
}
