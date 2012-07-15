package icfp
package emulator

// (xb to vp) renamed this just to States
// because now it's inside emulator, so it won't potentially interfere with Strategies
// fair enough?
trait States {
  self: Commands with Games with Items with Points with States with Worlds =>

  sealed trait State {
    def commands: Commands
    def terminal: Boolean = false
    def w: World
    def steps: Int = commands.size
    def collectedLambdas: Int
    def stepsUnderwater: Int = 0
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
      !w.liftIsBlockedForever && (Point(position.x, position.y - stepsToWater) distanceTo lift) <= timeToLiveUnderWater
    }
    def haveAllLambdas = w.remainingLambdas == 0

    override def equals(x: Any) = x.isInstanceOf[State] && {
      val other = x.asInstanceOf[State]
      position == other.position && w == other.w
    }
  }

  case class InProgress(w: World, commands: Commands, collectedLambdas: Int, suw: Int) extends State {
    def status = "in progress"
    override def stepsUnderwater = suw
    def step(c: Command): State = stepGame(this, c)
  }

  case class Lost(w: World, commands: Commands, collectedLambdas: Int) extends State {
    override def terminal = true
    def status = "lost"
  }

  case class Aborted(w: World, commands: Commands, collectedLambdas: Int) extends State {
    override def terminal = true
    def status = "aborted"
    override def score = super.score + 25 * collectedLambdas
  }

  case class Won(w: World, commands: Commands, collectedLambdas: Int) extends State {
    override def terminal = true
    def status = "won"
    override def score = super.score + 50 * collectedLambdas
  }
}