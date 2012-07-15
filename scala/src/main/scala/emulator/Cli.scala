package icfp
package emulator

trait Cli {
  self: Commands with Games with Items with Points with States with Worlds =>

  def runNonInteractive(g: State, commands: Commands) {
    println(g)
    val result = playGame(g, commands)
    println("%d of %d steps: %s".format(result.steps, commands.toList.length, (commands take result.steps).mkString))
    println(result)
  }

  def runInteractive(g: State): Commands = {
    var game = g.asInstanceOf[InProgress] // todo
    val moves = scala.collection.mutable.ListBuffer[Command]()

    var active = true
    while (active) {
      render(game)
      val c = jline.Terminal.getTerminal.readVirtualKey(System.in)
      val nextCommand = c match {
        case 'w' => Up
        case 's' => Down
        case 'a' => Left
        case 'd' => Right
        case ' ' => Abort
        case _ => Wait
      }
      val nextState = game.step(nextCommand)
      moves += nextCommand
      nextState match {
        case g: InProgress => game = g
        case _ =>
          render(nextState)
          active = false
      }
    }

    moves.toList
  }

  private def render(state: State) {
    val cr = new jline.ConsoleReader()
    cr.clearScreen()
    println(state.w)
    println("Score: %d (%s)".format(state.score, state.status))
  }
}