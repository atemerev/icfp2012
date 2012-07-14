package icfp
package emulator

trait Commands {
  self: Commands with Games with Items with Points with States with Worlds =>

  sealed trait Command {
    def dir: Point
  }

  object Command {
    def unapply(c: Char): Option[Command] = c match {
      case 'L' => Some(Left)
      case 'R' => Some(Right)
      case 'U' => Some(Up)
      case 'D' => Some(Down)
      case 'W' => Some(Wait)
      case 'A' => Some(Abort)
      case _ => None
    }
  }

  case object Left extends Command {
    def dir = (-1, 0)
    override def toString = "L"
  }

  case object Right extends Command {
    def dir = (1, 0)
    override def toString = "R"
  }

  case object Up extends Command {
    def dir = (0, 1)
    override def toString = "U"
  }

  case object Down extends Command {
    def dir = (0, -1)
    override def toString = "D"
  }

  case object Wait extends Command {
    def dir = (0, 0)
    override def toString = "W"
  }

  case object Abort extends Command {
    def dir = (0, 0)
    override def toString = "A"
  }

  type Commands = Traversable[Command]

  def mkCommands(line: String) = line map (Command.unapply(_).get)
}