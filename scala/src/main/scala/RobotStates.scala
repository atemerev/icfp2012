package icfp

trait RobotStates { self: Worlds =>
  abstract class State(val w: World, val steps: Int, collectedLambdas: Int) {
    def score = 25 * collectedLambdas - steps
    def status: String
    override def toString = "status = " + status + "\n" + w
  }

  case class Lost(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "lost"
  }

  case class Aborted(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "aborted"
    override def score = super.score + 25 * collectedLambdas
  }

  case class Won(override val w: World, override val steps: Int, collectedLambdas: Int) extends State(w, steps, collectedLambdas) {
    def status = "won"
    override def score = super.score + 50 * collectedLambdas
  }

}

