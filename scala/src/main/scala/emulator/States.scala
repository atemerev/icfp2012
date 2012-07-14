package icfp
package emulator

// (xb to vp) renamed this just to States
// because now it's inside emulator, so it won't potentially interfere with Strategies
// fair enough?
trait States {
  self: Emulator =>

  sealed trait State {
    def w: World
    def steps: Int
    def collectedLambdas: Int
    def stepsUnderwater: Int
    def score = 25 * collectedLambdas - steps
    def status: String
    override def toString = "status = " + status + "\n" + w
    def position = w.robot
    def lift = w.lift
    def distanceToLift = position distanceTo lift
    def timeToLiveUnderWater = if (w.flooding > 0) w.waterproof - stepsUnderwater else Int.MaxValue
    def underWater = w.isUnderwater
    def stepsToWater = if (underWater) 0 else
                       if (w.flooding == 0) position.y else
                       if (w.heightOverWater < w.timeToNextFlood) w.heightOverWater else
    {
      val dy = w.heightOverWater - w.timeToNextFlood
      val nFloods = dy / w.flooding
      w.heightOverWater - nFloods
    }

    def mayGetToLift: Boolean = { // this is simplistic; there can be more conditions, could add later
      (Point(position.x, position.y - stepsToWater) distanceTo lift) <= timeToLiveUnderWater
    }
  }

  case class InProgress(w: World, steps: Int, collectedLambdas: Int, stepsUnderwater: Int) extends State {
    def status = "in progress"
    def step(c: Command): State = stepGame(this, c)
  }

  case class Lost(w: World, steps: Int, collectedLambdas: Int) extends State {
    def stepsUnderwater = 0 // (xb to vp) is this really necessary?
    def status = "lost"
  }

  case class Aborted(w: World, steps: Int,  collectedLambdas: Int) extends State {
    def stepsUnderwater = 0 // (xb to vp) is this really necessary?
    def status = "aborted"
    override def score = super.score + 25 * collectedLambdas
  }

  case class Won(w: World, steps: Int, collectedLambdas: Int) extends State {
    def stepsUnderwater = 0 // (xb to vp) is this really necessary?
    def status = "won"
    override def score = super.score + 50 * collectedLambdas
  }
}